namespace HotChocolate.Data.ElasticSearch.Filters;

/// <inheritdoc />
public class ElasticFilterMetadataDescriptor : IElasticFilterMetadataDescriptor
{
    private readonly ElasticFilterMetadata _metadata;

    /// <summary>
    /// Initializes a new instance of <see cref="ElasticFilterMetadataDescriptor"/>
    /// </summary>
    public ElasticFilterMetadataDescriptor(ElasticFilterMetadata metadata)
    {
        _metadata = metadata;
    }

    /// <inheritdoc />
    public IElasticFilterMetadataDescriptor Name(string name)
    {
        _metadata.Name = name;
        return this;
    }

    /// <inheritdoc />
    public IElasticFilterMetadataDescriptor Kind(ElasticSearchOperationKind kind)
    {
        _metadata.Kind = kind;
        return this;
    }

    /// <inheritdoc />
    public IElasticFilterMetadataDescriptor AsFilter() => Kind(ElasticSearchOperationKind.Filter);

    /// <inheritdoc />
    public IElasticFilterMetadataDescriptor AsQuery() => Kind(ElasticSearchOperationKind.Query);

    /// <summary>
    /// Creates a new instance of <see cref="ElasticFilterMetadataDescriptor"/>
    /// </summary>
    public static IElasticFilterMetadataDescriptor Create(ElasticFilterMetadata metadata)
        => new ElasticFilterMetadataDescriptor(metadata);
}