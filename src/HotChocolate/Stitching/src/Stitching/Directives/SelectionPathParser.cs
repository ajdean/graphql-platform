using System.Buffers;
using System.Collections.Immutable;
using HotChocolate.Language;

namespace HotChocolate.Stitching;

internal static class SelectionPathParser
{
    private const int _maxStackSize = 256;

    public static IImmutableStack<SelectionPathComponent> Parse(string path)
    {
        if (path is null)
        {
            throw new ArgumentNullException(nameof(path));
        }

        byte[]? rented = null;
        var buffer = path.Length < _maxStackSize
            ? stackalloc byte[path.Length]
            : rented = ArrayPool<byte>.Shared.Rent(path.Length);

        try
        {
            buffer = buffer.Slice(0, path.Length);
            Prepare(path, buffer);
            var parser = new StitchingUtf8GraphQLParser(buffer);
            return ParseSelectionPath(ref parser);
        }
        finally
        {
            if (rented is not null)
            {
                buffer.Clear();
                ArrayPool<byte>.Shared.Return(rented);
            }
        }
    }

    private static void Prepare(string path, Span<byte> sourceText)
    {
        for (var i = 0; i < path.Length; i++)
        {
            var current = path[i];
            sourceText[i] = current == GraphQLConstants.Dot ? (byte)' ' : (byte)current;
        }
    }

    private static ImmutableStack<SelectionPathComponent> ParseSelectionPath(
        ref StitchingUtf8GraphQLParser parser)
    {
        var path = ImmutableStack<SelectionPathComponent>.Empty;

        parser.MoveNext();

        while (!parser.IsEndOfFile)
        {
            path = path.Push(ParseSelectionPathComponent(ref parser));
        }

        return path;
    }

    private static SelectionPathComponent ParseSelectionPathComponent(
        ref StitchingUtf8GraphQLParser parser)
    {
        var name = parser.ParseName();
        var arguments = ParseArguments(ref parser);
        return new SelectionPathComponent(name, arguments);
    }

    private static List<ArgumentNode> ParseArguments(
        ref StitchingUtf8GraphQLParser parser)
    {
        var list = new List<ArgumentNode>();

        if (parser.Reader.Kind == TokenKind.LeftParenthesis)
        {
            // skip opening token
            parser.MoveNext();

            while (parser.Reader.Kind != TokenKind.RightParenthesis)
            {
                list.Add(ParseArgument(ref parser));
            }

            // skip closing token
            parser.ExpectRightParenthesis();

        }
        return list;
    }

    private static ArgumentNode ParseArgument(ref StitchingUtf8GraphQLParser parser)
    {
        var name = parser.ParseName();

        parser.ExpectColon();

        var value = parser.ParseValueLiteral();

        return new ArgumentNode
        (
            null,
            name,
            value
        );
    }

}

