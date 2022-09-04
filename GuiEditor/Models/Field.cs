using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using Avalonia;
using Avalonia.Media.Imaging;
using Avalonia.Platform;
using SkiaSharp;

namespace GuiEditor.Models;

public class Field : IDisposable
{
    private Field(WriteableBitmap currentImage, IReadOnlyList<Block> blocks)
    {
        CurrentImage = currentImage;
        Blocks = blocks;
    }

    public static Field LoadFromPath(string path)
    {
        using var bmp = SKBitmap.Decode(path);
        var wb = new WriteableBitmap(new PixelSize(bmp.Width, bmp.Height), new Vector(96, 96), PixelFormat.Bgra8888,
            AlphaFormat.Opaque);
        using var fb = wb.Lock();
        GuiEditor.Render.DrawBitmap(fb, bmp);

        var configFileName = Path.ChangeExtension(path, "json");
        var blocks = ReadBlocks(configFileName);

        return new Field(wb, blocks);
    }

    private static IReadOnlyList<Block> ReadBlocks(string configFileName)
    {
        var content = File.ReadAllText(configFileName);
        var data = JsonSerializer.Deserialize<ConfigJson>(content, new JsonSerializerOptions()
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        })!;
        return data.Blocks.Select(jb => new Block
        {
            Id = jb.BlockId,
            X = jb.BottomLeft[0],
            Y = jb.TopRight[1],
            Width = jb.TopRight[0] - jb.BottomLeft[0],
            Height = jb.BottomLeft[1] - jb.TopRight[1]
        }).ToList();
    }
    
    public WriteableBitmap CurrentImage { get; }
    public IReadOnlyList<Block> Blocks { get; }

    public void Render(ILockedFramebuffer fb)
    {
        GuiEditor.Render.DrawBitmap(fb, CurrentImage);
        foreach (var block in Blocks)
        {
            GuiEditor.Render.DrawBlock(fb, block);
        }
    }

    public void Dispose()
    {
        CurrentImage.Dispose();
    }
}
