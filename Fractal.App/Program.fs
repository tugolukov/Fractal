open System
open System.Drawing
open System.Numerics
open System.Windows.Forms

(*
 * Тип для работы со свойствами фрактала
 *)
type Fractal =
    | Converge
    | Diverge of int

    (*
     * Вычисление квадрата комплексного числа
     *)
    static member Sqrt(c: Complex) = c.Real * c.Real + c.Imaginary * c.Imaginary

    (*
     * Уравнение фрактала
     *)
    static member FractalFunc (z: Complex) = z ** 7.0 + 0.626

    (*
     * Функция определения схождения уравнения
     *)
    static member Convergency z limit =
        let rec loop z iteration =
            if (Fractal.Sqrt z >= 1.5)
            then Diverge iteration
            elif iteration = limit
            then Converge
            else loop (Fractal.FractalFunc z) (iteration + 1)
        loop z 0

(*
 * Тип для работы цветом
 *)
type Colors =
    (*
     * Получение валидного значения для цветового канала
     *)
    static member GetValidValue private v =
        if (v <= 0) then 0
        elif (v >= 255) then 255
        else Math.Abs(v)

    (*
     * Получение цвета по индексу
     *)
    static member GetValidColor private v =
        Color.FromArgb(Colors.GetValidValue (30 + v), Colors.GetValidValue(50 + v), Colors.GetValidValue(70 + v))

    (*
     * Получение цвета по индексу
     *)
    static member GetColor v =
        if (v <= 0) then Colors.GetValidColor(0)
        elif (v > 0 && v <= 30) then Colors.GetValidColor(v * 2)
        else Color.FromArgb(245, 250, 255)

(*
 * Тип состояния формы
 *)
type FormState () = 
    let mutable zoom = 250
    let mutable center = Point(0, 0)

    member this.IterationLimit with get() = 100
    member this.Title with get() = "Fractal"
    member this.Width with get() = 800
    member this.Height with get() = 800
    member this.Zoom
        with get() = zoom
        and set(value: int) = zoom <- value
    member this.Center
        with set(value: Point) = center <- value

    (*
     * Получение цвета для пикселя
     *)
    member this.Color dx dy =
        match Fractal.Convergency (Complex(dx, dy)) this.IterationLimit with
            | Converge -> Color.FromArgb(0, 0, 0)
            | Diverge d -> Colors.GetColor d
    

    (*
     * Расчет пикселя в пространстве
     *)
    member this.PixelColor x y = 
        this.Color
            (((float)(x - (this.Width / 2) + center.X))/(float) zoom)
            (((float)(y - (this.Height / 2) + center.Y))/(float) zoom)

    (*
     * Строка состояния, которая выводится на форму
     *)
    member this.ToString =
        String.Format("Zoom: {0}; Relative to X: {1}, Y: {2}", zoom, center.X, center.Y)

(*
 * Тип формы
 *)
type FormFractal (state: FormState) =
    inherit Form()
    
    do 
        base.Text <- state.Title
        base.Height <- state.Height
        base.Width <- state.Width
    let state = state
    member this.State with get() = state

    (*
     * Изменение масштаба
     *)   
    member this.ScaleUp (delta: int) = 
        this.State.Zoom <- this.State.Zoom + delta
    member this.ScaleDown (delta: int) =     
        this.State.Zoom <- this.State.Zoom + delta 

    (*
     * Получение изображения с фракталом
     *)
    member this.GetBitmap = 
        let bitmap = new Bitmap(state.Width, state.Height, Imaging.PixelFormat.Format24bppRgb)
        for y0 in [0..state.Height - 1] do
            for x0 in [0..state.Width - 1] do
                bitmap.SetPixel(x0, y0, this.State.PixelColor x0 y0 )
        bitmap

    (*
     * Перерисовка изображения
     *)
    member this.Redraw() =
        this.Invalidate()
        this.Cursor <- Cursors.WaitCursor
        this.BackgroundImage <- this.GetBitmap
        this.Cursor <- Cursors.Default
        this.Paint.Add(fun e ->
            e.Graphics.DrawString(this.State.ToString, new Font("Helvetica", 10.0f), new SolidBrush(Color.White), PointF(10.0f, 10.0f)))

    (*
     * Действия на кручение колеса мыши
     *)
    override this.OnMouseWheel e =
        match e.Delta with
            | d when d > 0 -> this.ScaleDown(d)  
            | d when d <= 0 -> this.ScaleUp(d)
        this.Redraw()

    (*
     * Действия на поднятие кнопки мыши
     *)    
    override this.OnMouseUp e =
        state.Center <- Point(400 - e.X, 400 - e.Y)
        this.Redraw()


let form = new FormFractal (new FormState())
form.Redraw()
do Application.Run(form)