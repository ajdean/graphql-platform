using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Threading.Tasks;
using HotChocolate.Language;
using Microsoft.Extensions.DependencyInjection;

namespace HotChocolate.Execution.Pipeline;

internal sealed class WritePersistedQueryMiddleware
{
    private const string _persistedQuery = "persistedQuery";
    private const string _persisted = "persisted";
    private const string _expectedValue = "expectedHashValue";
    private const string _expectedType = "expectedHashType";
    private const string _expectedFormat = "expectedHashFormat";
    private readonly RequestDelegate _next;
    private readonly IDocumentHashProvider _hashProvider;
    private readonly IOperationDocumentStore _operationDocumentStore;

    private WritePersistedQueryMiddleware(RequestDelegate next,
        IDocumentHashProvider documentHashProvider,
        [SchemaService] IOperationDocumentStore operationDocumentStore)
    {
        _next = next ??
            throw new ArgumentNullException(nameof(next));
        _hashProvider = documentHashProvider ??
            throw new ArgumentNullException(nameof(documentHashProvider));
        _operationDocumentStore = operationDocumentStore ??
            throw new ArgumentNullException(nameof(operationDocumentStore));
    }

    public async ValueTask InvokeAsync(IRequestContext context)
    {
        await _next(context).ConfigureAwait(false);

        if (!context.IsCachedDocument &&
            context.Document is not null &&
            context.DocumentId is { } documentId &&
            context.Request.Document is { } document &&
            context.Result is IOperationResult result &&
            context.IsValidDocument &&
            context.Request.Extensions is not null &&
            context.Request.Extensions.TryGetValue(_persistedQuery, out var s) &&
            s is IReadOnlyDictionary<string, object> settings)
        {
            var resultBuilder = OperationResultBuilder.FromResult(result);

            // hash is found and matches the query key -> store the query
            if (DoHashesMatch(settings, documentId, _hashProvider.Name, out var userHash))
            {
                // save the query
                await _operationDocumentStore.SaveAsync(documentId, document).ConfigureAwait(false);

                // add persistence receipt to the result
                resultBuilder.SetExtension(
                    _persistedQuery,
                    new Dictionary<string, object>
                    {
                        { _hashProvider.Name, userHash },
                        { _persisted, true },
                    });

                context.ContextData[WellKnownContextData.DocumentSaved] = true;
            }
            else
            {
                resultBuilder.SetExtension(
                    _persistedQuery,
                    new Dictionary<string, object?>
                    {
                        { _hashProvider.Name, userHash },
                        { _expectedValue, context.DocumentId },
                        { _expectedType, _hashProvider.Name },
                        { _expectedFormat, _hashProvider.Format.ToString() },
                        { _persisted, false },
                    });
            }

            context.Result = resultBuilder.Build();
        }
    }

    private static bool DoHashesMatch(
        IReadOnlyDictionary<string, object> settings,
        OperationDocumentId expectedHash,
        string hashName,
        [NotNullWhen(true)] out string? userHash)
    {
        if (settings.TryGetValue(hashName, out var h) &&
            h is string hash)
        {
            userHash = hash;
            return hash.Equals(expectedHash.Value, StringComparison.Ordinal);
        }

        userHash = null;
        return false;
    }

    public static RequestCoreMiddleware Create()
        => (core, next) =>
        {
            var documentHashProvider = core.Services.GetRequiredService<IDocumentHashProvider>();
            var persistedQueryStore = core.SchemaServices.GetRequiredService<IOperationDocumentStore>();
            var middleware = new WritePersistedQueryMiddleware(next, documentHashProvider, persistedQueryStore);
            return context => middleware.InvokeAsync(context);
        };
}
