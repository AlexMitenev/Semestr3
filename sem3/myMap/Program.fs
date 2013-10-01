// This program can consider the distances of points you click on map

open System
open System.Windows.Forms
open System.Drawing

let mainForm =
 
    let mutable mapFactorKoef = 1.0
    //BUTTONS and TEXTBOX

    let form = new Form(Text = "Main form", Width = 640, Height = 500)
                                
    let up = new Button(Text = "up", Left = 500,
                    Top = 100, Width = 80)

    let down = new Button(Text = "down", Left = 500,
                    Top = 120, Width = 80)
    
    let left = new Button(Text = "left", Left = 500,
                    Top = 140, Width = 80)

    let right = new Button(Text = "right", Left = 500,
                    Top = 160, Width = 80)

    let plus = new Button(Text = "+", Left = 500, Enabled = false,
                    Top = 180, Width = 80)

    let minus = new Button(Text = "-", Left = 500, Enabled = false,
                    Top = 200, Width = 80)

    let coords = new TextBox(Text = "", Width = 80, Enabled = false,
                            Top = 250, Left = 500)                

    let sumDistanse = new TextBox(Text = "0.0", Width = 80, Enabled = false,
                            Top = 280, Left = 500) 
    //Picture

    let map = new PictureBox(Height = 1500, Width = 1500)
    let img = Image.FromFile("map.jpg")
    map.BackgroundImage <- img

    // Buttons Clicks
    let step = 20
    let mapSize = 800

    up.Click.Add(fun _ -> 
        match map.Top with
            |0 -> ()
            |n -> map.Top <- map.Top + step)

    down.Click.Add(fun _ -> 
        match map.Top with
            |n when n > -(mapSize) -> map.Top <- map.Top - step
            |n -> ())

    left.Click.Add(fun _ -> 
        match map.Left with
            |0 -> ()
            |n -> map.Left <- map.Left + step)

    right.Click.Add(fun _ -> 
        match map.Left with
            |n when n > -(mapSize) -> map.Left <- map.Left - step
            |n -> ())
      
    //Coords
    let getCoords (a : MouseEventArgs) = coords.Text <- string (a.X) + "," + string(a.Y)
    map.MouseMove |> Event.add getCoords

    //Sum of Distanses
    let redPen = new Pen(Color.Firebrick, 1.0f)
    
    let sumDist (a : MouseEventArgs, b : MouseEventArgs) = 
        let sum = 
            float ((a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)) |>
            sqrt |> 
            (+) (float sumDistanse.Text)|>
            string
        sumDistanse.Text  <- sum
        let painting (e : PaintEventArgs) = e.Graphics.DrawLine(redPen, Point(a.X,a.Y), Point(b.X, b.Y))
        map.Paint.Add painting
    
    map.MouseClick |> Event.pairwise |> Event.add sumDist
    map.MouseClick |> Event.add (fun _ -> map.Refresh())

    form.Controls.AddRange([|up; down; left; right; plus; minus;
                             coords; sumDistanse; map|])
    form

do Application.Run(mainForm)