using System.Text;
using CookieCrumble;
using Xunit;

namespace HotChocolate.Language;

public class Sha256DocumentHashProviderTests
{
    [Fact]
    public void HashAsBase64()
    {
        var content = Encoding.UTF8.GetBytes("abc");
        var hashProvider = new Sha256DocumentHashProvider(HashFormat.Base64);

        var hash = hashProvider.ComputeHash(content);

        Snapshot
            .Create()
            .Add(hash)
            .MatchInline("ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0=");
    }

    [Fact]
    public void HashAsHex()
    {
        var content = Encoding.UTF8.GetBytes("abc");
        var hashProvider = new Sha256DocumentHashProvider(HashFormat.Hex);

        var hash = hashProvider.ComputeHash(content);

        Snapshot
            .Create()
            .Add(hash)
            .MatchInline("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    }
}
