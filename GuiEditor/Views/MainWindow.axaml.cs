using Avalonia.Controls;
using Avalonia.Input;
using GuiEditor.ViewModels;

namespace GuiEditor.Views
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void OnPointerPressed(object? sender, PointerPressedEventArgs e)
        {
            ((MainWindowViewModel)DataContext).ImagePointerPressed((Image)sender, e);
        }
    }
}