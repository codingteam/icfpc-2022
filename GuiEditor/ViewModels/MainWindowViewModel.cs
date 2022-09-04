using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Media.Imaging;
using Avalonia.Platform;
using Avalonia.Skia;
using GuiEditor.Models;
using JetBrains.Annotations;
using SkiaSharp;

namespace GuiEditor.ViewModels;

public class MainWindowViewModel : INotifyPropertyChanged
{
    private static string GetInitialImagePath()
    {
        var currentDir = Assembly.GetExecutingAssembly().Location;
        while (!Directory.Exists(Path.Combine(currentDir, "problems")))
        {
            currentDir = Path.GetDirectoryName(currentDir);
        }

        return Path.Combine(currentDir, "problems/27.initial.png");
    }
        
    private WriteableBitmap? _bitmap;
    private string _info;

    public WriteableBitmap? Bitmap
    {
        get => _bitmap;
        set
        {
            _bitmap = value;
            OnPropertyChanged();
        }
    }

    public string InitialImagePath { get; set; } = GetInitialImagePath();

    private Field? _field;

    public string Info
    {
        get => _info;
        set
        {
            if (value == _info) return;
            _info = value;
            OnPropertyChanged();
        }
    }

    private (double, double) TranslatePosition(PointerEventArgs ea, Image image)
    {
        var pos = ea.GetPosition(image);
        var imageSize = (image.Bounds.Width, image.Bounds.Height);
        var sourceSize = Bitmap.PixelSize;
        var relativePos = (x: pos.X / imageSize.Width, y: pos.Y / imageSize.Height);
        var actualPos = (relativePos.x * sourceSize.Width, relativePos.y * sourceSize.Height);
        return actualPos;
    }
        
    public void ImagePointerPressed(Image sender, PointerEventArgs e)
    {
        var (x, y) = TranslatePosition(e, sender);
        Info = $"Chosen coords: {(int)x}, {(int)y}";
    }
        
    public void LoadInitialImage()
    {
        using var stream = new FileStream(InitialImagePath, FileMode.Open);
        using (_field) _field = Field.LoadFromPath(InitialImagePath);
        // using (Bitmap) Bitmap = _field.CurrentImage;
        var bmp = _field.CurrentImage;
        using (Bitmap) Bitmap = new WriteableBitmap(bmp.PixelSize, bmp.Dpi, PixelFormat.Bgra8888, AlphaFormat.Opaque);
        using var fb = Bitmap.Lock();
        Render.DrawBitmap(fb, bmp);
    }

    public void CutHorizontal()
    { 
        // TODO
    }
        
    public event PropertyChangedEventHandler? PropertyChanged;

    [NotifyPropertyChangedInvocator]
    protected virtual void OnPropertyChanged(
        [CallerMemberName] string propertyName = null)
    {
        PropertyChanged?.Invoke(this,
            new PropertyChangedEventArgs(propertyName));
    }
}