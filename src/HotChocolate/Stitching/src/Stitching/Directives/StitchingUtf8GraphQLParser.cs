using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using HotChocolate.Language;

namespace HotChocolate.Stitching;

public ref struct StitchingUtf8GraphQLParser
{
    private Utf8GraphQLReader _reader;
    private readonly bool _createLocation;
    private int _parsedNodes;
    private readonly int _maxAllowedNodes;

    public StitchingUtf8GraphQLParser(ReadOnlySpan<byte> buffer)
    {
        var options = new ParserOptions();
        _reader = new Utf8GraphQLReader(buffer);
        _createLocation = !options.NoLocations;
        _maxAllowedNodes = options.MaxAllowedNodes;
    }

    public Utf8GraphQLReader Reader { get => _reader; }

    public bool IsEndOfFile => _reader.Kind is TokenKind.EndOfFile;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal bool MoveNext() => _reader.MoveNext();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public NameNode ParseName()
    {
        var start = Start();
        var name = ExpectName();
        var location = CreateLocation(in start);

        return new NameNode
        (
            location,
            name
        );
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void ExpectColon() => Expect(TokenKind.Colon);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void ExpectRightParenthesis() => Expect(TokenKind.RightParenthesis);

    public IValueNode ParseValueLiteral()
    {
        if (_reader.Kind == TokenKind.LeftBracket)
        {
            return ParseList();
        }

        if (_reader.Kind == TokenKind.LeftBrace)
        {
            return ParseObject();
        }

        if (TokenHelper.IsScalarValue(in _reader))
        {
            return ParseScalarValue();
        }

        if (_reader.Kind == TokenKind.Name)
        {
            return ParseEnumValue();
        }

        if (_reader.Kind == TokenKind.Dollar)
        {
            return ParseVariable();
        }

        throw Unexpected(_reader.Kind);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private ListValueNode ParseList()
    {
        var start = Start();

        if (_reader.Kind != TokenKind.LeftBracket)
        {
            throw new SyntaxException(
                _reader,
                "Expected a {0} token but found `{1}`.",
                TokenKind.LeftBracket,
                _reader.Kind);
        }

        var items = new List<IValueNode>();

        // skip opening token
        _reader.MoveNext();

        while (_reader.Kind != TokenKind.RightBracket)
        {
            items.Add(ParseValueLiteral());
        }

        // skip closing token
        Expect(TokenKind.RightBracket);

        var location = CreateLocation(in start);

        return new ListValueNode
        (
            location,
            items
        );
    }

    private ObjectValueNode ParseObject()
    {
        var start = Start();

        if (_reader.Kind != TokenKind.LeftBrace)
        {
            throw new SyntaxException(
                _reader,
                "Expected a {0} token but found `{1}`.",
                TokenKind.LeftBrace,
                _reader.Kind);
        }

        var fields = new List<ObjectFieldNode>();

        // skip opening token
        _reader.MoveNext();

        while (_reader.Kind != TokenKind.RightBrace)
        {
            var fieldStart = Start();
            var name = ParseName();
            ExpectColon();
            var value = ParseValueLiteral();
            var fieldLocation = CreateLocation(in fieldStart);

            fields.Add(new ObjectFieldNode(fieldLocation, name, value));
        }

        // skip closing token
        Expect(TokenKind.RightBrace);

        var location = CreateLocation(in start);

        return new ObjectValueNode
        (
            location,
            fields
        );
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private IValueNode ParseScalarValue()
    {
        if (TokenHelper.IsString(in _reader))
        {
            return ParseStringLiteral();
        }

        var start = Start();
        var kind = _reader.Kind;

        if (!TokenHelper.IsScalarValue(in _reader))
        {
            throw new SyntaxException(_reader,
                "Expected a `Int`-, `Float`-, `String`- or `BlockString`-token, but found a `{0}`-token.",
                _reader.Kind);
        }

        ReadOnlyMemory<byte> value = _reader.Value.ToArray();
        var format = _reader.FloatFormat;
        _reader.MoveNext();

        var location = CreateLocation(in start);

        if (kind == TokenKind.Float)
        {
            return new FloatValueNode
            (
                location,
                value,
                format ?? FloatFormat.FixedPoint
            );
        }

        if (kind == TokenKind.Integer)
        {
            return new IntValueNode
            (
                location,
                value
            );
        }

        throw Unexpected(kind);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private IValueNode ParseEnumValue()
    {
        var start = Start();

        HotChocolate.Language.Location? location;

        if (_reader.Value.SequenceEqual(GraphQLKeywords.True))
        {
            _reader.MoveNext();
            location = CreateLocation(in start);
            return new BooleanValueNode(location, true);
        }

        if (_reader.Value.SequenceEqual(GraphQLKeywords.False))
        {
            _reader.MoveNext();
            location = CreateLocation(in start);
            return new BooleanValueNode(location, false);
        }

        if (_reader.Value.SequenceEqual(GraphQLKeywords.Null))
        {
            _reader.MoveNext();
            if (_createLocation)
            {
                location = CreateLocation(in start);
                return new NullValueNode(location);
            }

            return NullValueNode.Default;
        }

        var value = _reader.GetString();
        _reader.MoveNext();
        location = CreateLocation(in start);

        return new EnumValueNode
        (
            location,
            value
        );
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private StringValueNode ParseStringLiteral()
    {
        var start = Start();

        var isBlock = _reader.Kind == TokenKind.BlockString;
        var value = ExpectString();
        var location = CreateLocation(in start);

        return new StringValueNode(location, value, isBlock);
    }

    private ScopedVariableNode ParseVariable()
    {
        ExpectDollar();
        var scope = ParseName();
        ExpectColon();
        var name = ParseName();

        return new ScopedVariableNode
        (
            null,
            scope,
            name
        );
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private TokenInfo Start()
    {
        if (++_parsedNodes > _maxAllowedNodes)
        {
            throw new SyntaxException(
                _reader,
                string.Format(
                    "Document contains more than {0} nodes. Parsing aborted.",
                    _maxAllowedNodes));
        }

        return _createLocation
            ? new TokenInfo(
                _reader.Start,
                _reader.End,
                _reader.Line,
                _reader.Column)
            : default;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private HotChocolate.Language.Location? CreateLocation(in TokenInfo start) =>
        _createLocation
            ? new HotChocolate.Language.Location(
                start.Start,
                _reader.End,
                start.Line,
                start.Column)
            : null;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private string ExpectString()
    {
        if (TokenHelper.IsString(in _reader))
        {
            var value = _reader.GetString();
            _reader.MoveNext();
            return value;
        }

        throw new SyntaxException(_reader, "Expected a `{0}`-token, but found a `{1}`-token.", TokenKind.String,
            _reader.Kind);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void ExpectDollar() => Expect(TokenKind.Dollar);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private string ExpectName()
    {
        if (_reader.Kind == TokenKind.Name)
        {
            var name = _reader.GetName();
            _reader.MoveNext();
            return name;
        }

        throw new SyntaxException(_reader, "Expected a `{0}`-token, but found a `{1}`-token.", TokenKind.Name,
            _reader.Kind);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void Expect(TokenKind kind)
    {
        if (!_reader.Skip(kind))
        {
            throw new SyntaxException(_reader, "Expected a `{0}`-token, but found a `{1}`-token.", kind, _reader.Kind);
        }
    }

    private SyntaxException Unexpected(TokenKind kind)
        => new(_reader, "Unexpected token: {0}", kind.ToString());

    private static class TokenHelper
    {
        private static readonly bool[] _isString = new bool[22];
        private static readonly bool[] _isScalar = new bool[22];

        static TokenHelper()
        {
            _isString[(int)TokenKind.BlockString] = true;
            _isString[(int)TokenKind.String] = true;

            _isScalar[(int)TokenKind.BlockString] = true;
            _isScalar[(int)TokenKind.String] = true;
            _isScalar[(int)TokenKind.Integer] = true;
            _isScalar[(int)TokenKind.Float] = true;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsDescription(in Utf8GraphQLReader reader)
        {
            ref var searchSpace = ref MemoryMarshal.GetReference(_isString.AsSpan());
            var index = (int)reader.Kind;
            return Unsafe.Add(ref searchSpace, index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsString(in Utf8GraphQLReader reader)
        {
            ref var searchSpace = ref MemoryMarshal.GetReference(_isString.AsSpan());
            var index = (int)reader.Kind;
            return Unsafe.Add(ref searchSpace, index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsScalarValue(in Utf8GraphQLReader reader)
        {
            ref var searchSpace = ref MemoryMarshal.GetReference(_isScalar.AsSpan());
            var index = (int)reader.Kind;
            return Unsafe.Add(ref searchSpace, index);
        }
    }

    private ref struct TokenInfo
    {
        public TokenInfo(int start, int end, int line, int column)
        {
            Start = start;
            End = end;
            Line = line;
            Column = column;
        }

        /// <summary>
        /// Gets the character offset at which this
        /// <see cref="ISyntaxNode" /> begins.
        /// </summary>
        public int Start { get; }

        /// <summary>
        /// Gets the character offset at which this
        /// <see cref="ISyntaxNode" /> ends.
        /// </summary>
        public int End { get; }

        /// <summary>
        /// Gets the 1-indexed line number on which this
        /// <see cref="ISyntaxNode" /> appears.
        /// </summary>
        public int Line { get; }

        /// <summary>
        /// Gets the 1-indexed column number at which this
        /// <see cref="ISyntaxNode" /> begins.
        /// </summary>
        public int Column { get; }
    }
}
