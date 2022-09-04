using System;
using System.Collections.Generic;

namespace GuiEditor.Models;

public class ConfigJson
{
    public int Width { get; set; }
    public int Height { get; set; }
    public IEnumerable<BlockJson> Blocks { get; set; }
}

//{"blockId": "0", "bottomLeft": [0, 0], "topRight": [20, 20], "color": [0, 74, 173, 255]
public class BlockJson
{
    public string BlockId { get; set; } 
    public int[] BottomLeft { get; set; }
    public int[] TopRight { get; set; }
    public int[] Color { get; set; }
}