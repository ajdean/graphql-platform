using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using CookieCrumble;
using HotChocolate.Execution;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Moq;

namespace HotChocolate.Data;

public class EntityFrameworkResolverCompilerIntegrationTests
{
    [Fact]
    public async Task Resolver_Pipeline_With_DbContext_Is_Created()
    {
        using AuthorFixture authorFixture = new();

        var contextFactory = new Mock<IDbContextFactory<BookContext>>();

        contextFactory
            .Setup(t => t.CreateDbContextAsync(It.IsAny<CancellationToken>()))
            .Returns(Task.FromResult(authorFixture.Context));

        var result = await new ServiceCollection()
            .AddSingleton(contextFactory.Object)
            .AddGraphQL()
            .AddQueryType<Query>()
            .RegisterDbContext<BookContext>(DbContextKind.Pooled)
            .ExecuteRequestAsync("{ books { title } }");

        result.MatchSnapshot();
    }

    [Fact]
    public async Task Resolver_Pipeline_With_Request_DbContext_Is_Created()
    {
        using AuthorFixture authorFixture = new();

        using var scope = new ServiceCollection()
            .AddScoped(_ => authorFixture.Context)
            .AddGraphQL()
            .AddQueryType<Query>()
            .RegisterDbContext<BookContext>(DbContextKind.Synchronized)
            .ModifyRequestOptions(o => o.IncludeExceptionDetails = true)
            .Services
            .BuildServiceProvider()
            .CreateScope();

        var result = await scope.ServiceProvider.ExecuteRequestAsync(
            QueryRequestBuilder.New()
                .SetQuery("{ books { title } }")
                .SetServices(scope.ServiceProvider)
                .Create());

        result.MatchSnapshot();
    }

    [Fact]
    public async Task Resolver_Pipeline_With_Field_DbContext_Is_Created()
    {
        using AuthorFixture authorFixture = new();

        await using var service = new ServiceCollection()
            .AddScoped(_ => authorFixture.Context)
            .AddGraphQL()
            .AddQueryType<Query>()
            .RegisterDbContext<BookContext>(DbContextKind.Resolver)
            .Services
            .BuildServiceProvider();

        var result = await service.ExecuteRequestAsync(
            QueryRequestBuilder.New()
                .SetQuery("{ books { title } }")
                .SetServices(service)
                .Create());

        result.MatchSnapshot();
    }

    public class Query
    {
        public IQueryable<Book> GetBooks(BookContext context)
            => context.Books;
    }
}
