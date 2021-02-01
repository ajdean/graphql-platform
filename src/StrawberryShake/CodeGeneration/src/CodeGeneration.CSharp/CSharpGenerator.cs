using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.Extensions.DependencyInjection;
using HotChocolate;
using HotChocolate.Language;
using HotChocolate.Validation;
using StrawberryShake.CodeGeneration.Analyzers;
using StrawberryShake.CodeGeneration.Analyzers.Models;
using StrawberryShake.CodeGeneration.Utilities;
using static StrawberryShake.CodeGeneration.CodeGenerationThrowHelper;

namespace StrawberryShake.CodeGeneration.CSharp
{
    public class CSharpGenerator
    {
        public CSharpGeneratorResult Generate(IEnumerable<string> graphQLFiles)
        {
            if (graphQLFiles is null)
            {
                throw new ArgumentNullException(nameof(graphQLFiles));
            }

            var errors = new List<IError>();
            var documents = new List<DocumentNode>();

            foreach (var file in graphQLFiles)
            {
                try
                {
                    documents.Add(Utf8GraphQLParser.Parse(File.ReadAllBytes(file)));
                }
                catch (SyntaxException syntaxException)
                {
                    errors.Add(Generator_SyntaxException(syntaxException));
                }
            }

            if (errors.Any())
            {
                return new CSharpGeneratorResult(
                    new List<CSharpDocument>(),
                    errors);
            }

            var typeSystemDocs = documents.GetTypeSystemDocuments().ToList();
            var executableDocs = documents.GetExecutableDocuments().ToList();

            if (typeSystemDocs.Count == 0)
            {
                errors.AddRange(Generator_NoTypeDocumentsFound());
            }

            if (executableDocs.Count == 0)
            {
                errors.AddRange(Generator_NoExecutableDocumentsFound());
            }

            if (errors.Any())
            {
                return new CSharpGeneratorResult(
                    new List<CSharpDocument>(),
                    errors);
            }

            ISchema schema = SchemaHelper.Load(typeSystemDocs);
            IDocumentValidator validator = new ServiceCollection()
                .AddValidation()
                .Services
                .BuildServiceProvider()
                .GetRequiredService<IDocumentValidatorFactory>()
                .CreateValidator();

            foreach (var result in executableDocs
                .Select(
                    operation => validator.Validate(
                        schema,
                        operation))
                .Where(result => result.HasErrors))
            {
                errors.AddRange(result.Errors);
            }

            var analyzer = new DocumentAnalyzer();
            analyzer.SetSchema(schema);

            foreach (DocumentNode executableDocument in executableDocs)
            {
                analyzer.AddDocument(executableDocument);
            }

            ClientModel clientModel = analyzer.Analyze();

            var executor = new CSharpGeneratorExecutor();

            return new CSharpGeneratorResult(
                executor.Generate(
                    clientModel,
                    "Foo",
                    "FooClient").ToList(),
                errors);
        }
    }
}
