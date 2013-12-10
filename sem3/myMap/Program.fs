// This program can consider the distances of points you click on map

open System
open System.Windows.Forms
open System.Drawing

type Creator() =
        let Left = 500
        let Width = 80
        let TBEnabled = false
        member this.TBFactory text top = 
            new TextBox(Text = text, Width = Width, Enabled = TBEnabled,
                        Top = top, Left = Left)   
        member this.ButtonFactory text top = 
            new Button(Text = text, Left = Left,
                   Top = top, Width = Width)

let mainForm =

    let form = new Form(Text = "Main form", Width = 640, Height = 500)

    //BUTTONS and TEXTBOX
    let cr = new Creator()                    
                                  
    let up    = cr.ButtonFactory "^" 100
    let down  = cr.ButtonFactory "v" 120
    let left  = cr.ButtonFactory "<" 140
    let right = cr.ButtonFactory ">" 160
    let plus  = cr.ButtonFactory "+" 180
    let minus = cr.ButtonFactory "-" 200

    let coords = cr.TBFactory "" 250              
    let sumDistanse = cr.TBFactory "0.0" 280 

    //Picture

    let map = new PictureBox(Height = 1500, Width = 1500)
    map.SizeMode <-  PictureBoxSizeMode.StretchImage
    let img = Image.FromFile("map.jpg")   
    map.Image <- img

    // Buttons Clicks
    let step    = 20
    let minMapSize = 700
    let maxMapSize = 4000

    up.Click.Add(fun _ -> 
        match map.Top with
            |0 -> ()
            |n -> map.Top <- map.Top + step)

    down.Click.Add(fun _ -> 
        match map.Top with
            |n when n > -(map.Height - form.Height) -> map.Top <- map.Top - step
            |n -> ())

    left.Click.Add(fun _ -> 
        match map.Left with
            |0 -> ()
            |n -> map.Left <- map.Left + step)

    right.Click.Add(fun _ -> 
        match map.Left with
            |n when n > -(map.Width - form.Width) -> map.Left <- map.Left - step
            |n -> ())

    minus.Click.Add(fun _ -> 
        match map.Height with
        | n when n > minMapSize ->
            map.Height <- map.Height - step
            map.Width <- map.Width - step
        | _ -> ())

    plus.Click.Add(fun _ -> 
        match map.Height with
        | n when n < maxMapSize ->
            map.Height <- map.Height + step
            map.Width <- map.Width + step
        | _ -> ())

    //Coords
    let getCoords (a : MouseEventArgs) = coords.Text <- string (a.X) + ", " + string(a.Y)
    map.MouseMove |> Event.add getCoords

    //Sum of Distanses
    let redPen = new Pen(Color.Firebrick, 2.0f)
    
    let sumDist (a : MouseEventArgs, b : MouseEventArgs) = 
        let sum = 
            float ((a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)) 
            |> sqrt  
            |> (+) (float sumDistanse.Text)
            |> string
        let painting (e : PaintEventArgs) = e.Graphics.DrawLine(redPen, Point(a.X,a.Y), Point(b.X, b.Y))

        sumDistanse.Text  <- sum
        map.Paint.Add painting

    map.MouseClick |> Event.pairwise |> Event.add sumDist
    map.MouseClick |> Event.add (fun _ -> map.Refresh())

    form.Controls.AddRange([|up; down; left; right; plus; minus;
                             coords; sumDistanse; map|])
    form

do Application.Run(mainForm)