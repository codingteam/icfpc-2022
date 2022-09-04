using Avalonia.Media.Imaging;
using Avalonia.Platform;
using Avalonia.Skia;
using GuiEditor.Models;
using SkiaSharp;

namespace GuiEditor;

public static class Render
{
    public static void DrawBitmap(ILockedFramebuffer fb, SKBitmap bmp)
    {
        var targetInfo = new SKImageInfo(fb.Size.Width, fb.Size.Height,
            fb.Format.ToSkColorType(),SKAlphaType.Premul);
        using var surface = SKSurface.Create(targetInfo, fb.Address, fb.RowBytes);
        var canvas = surface.Canvas;
        canvas.DrawBitmap(bmp, 0, 0);
    }
    
    public static void DrawBitmap(ILockedFramebuffer fb, WriteableBitmap bmp)
    {
        using var sourceFb = bmp.Lock();
        var sourceInfo = new SKImageInfo(sourceFb.Size.Width, sourceFb.Size.Height, sourceFb.Format.ToSkColorType(),
            SKAlphaType.Premul);
        var sourceImage = SKImage.FromPixels(sourceInfo, sourceFb.Address);
            
        var targetInfo = new SKImageInfo(fb.Size.Width, fb.Size.Height,
            fb.Format.ToSkColorType(),SKAlphaType.Premul);
        using var surface = SKSurface.Create(targetInfo, fb.Address, fb.RowBytes);
        var canvas = surface.Canvas;
        canvas.DrawImage(sourceImage, 0, 0);
    }

    public static void DrawBlock(ILockedFramebuffer fb, Block block)
    {
        var info = new SKImageInfo(fb.Size.Width, fb.Size.Height,
            fb.Format.ToSkColorType(),SKAlphaType.Premul);
        using var surface = SKSurface.Create(info, fb.Address, fb.RowBytes);
        var canvas = surface.Canvas;
        canvas.DrawRect(block.X, block.Y, block.Width, block.Height, new SKPaint {Color = new SKColor(50, 50, 50, 50)});
    }
}
