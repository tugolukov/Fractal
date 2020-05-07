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
    static member FractalFunc (z: Complex) = (z + Complex(0.54, 0.54)) ** 4.0

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
        Color.FromArgb(Colors.GetValidValue (50 + v), Colors.GetValidValue(140 + v), Colors.GetValidValue(100 + v))

    (*
     * Получение цвета по индексу
     *)
    static member GetColor e =
        if (e <= 0)
            then System.Drawing.Color.FromArgb(255, 255, 255)
        elif (e > 0 && e <= 1)
            then System.Drawing.Color.FromArgb(30, 20, 10)
        elif (e > 1 && e <= 2)
            then System.Drawing.Color.FromArgb(30, 40, 50)
        elif (e > 2 && e <= 3)
            then System.Drawing.Color.FromArgb(70, 60, 50)  
        elif (e > 3 && e <= 4)
            then System.Drawing.Color.FromArgb(70, 80, 90)
        elif (e > 4 && e <= 5)
            then System.Drawing.Color.FromArgb(110, 100, 90)
        elif (e > 5 && e <= 6)
            then System.Drawing.Color.FromArgb(110, 120, 130)
        elif (e > 6 && e <= 7)
            then System.Drawing.Color.FromArgb(150, 140, 130)
        elif (e > 7 && e <= 8)
            then System.Drawing.Color.FromArgb(150, 160, 170)
        elif (e > 8 && e <= 9)
            then System.Drawing.Color.FromArgb(190, 180, 170)
        elif (e > 9 && e <= 10)
            then System.Drawing.Color.FromArgb(190, 200, 210)
        elif (e > 10 && e <= 11)
            then System.Drawing.Color.FromArgb(230, 220, 210)
        elif (e > 11 && e <= 12)
            then System.Drawing.Color.FromArgb(230, 240, 250)
        elif (e > 12 && e <= 13)
            then System.Drawing.Color.FromArgb(190, 210, 250)
        elif (e > 13 && e <= 14)
            then System.Drawing.Color.FromArgb(190, 180, 170)
        elif (e>14 && e<=15)
            then System.Drawing.Color.FromArgb(150, 160, 170)
        elif (e>15 && e<=16)
            then System.Drawing.Color.FromArgb(150, 140, 130)
        elif (e>16 && e<=18)
            then System.Drawing.Color.FromArgb(110, 120, 130)
        elif (e>18 && e<=20)
            then System.Drawing.Color.FromArgb(110, 100, 90)
        elif (e>20 && e<=22)
            then System.Drawing.Color.FromArgb(70, 80, 90)
        elif (e>22 && e<=24)
            then System.Drawing.Color.FromArgb(70, 60, 50)
        elif (e>24 && e<=26)
            then System.Drawing.Color.FromArgb(40, 30, 20)
        elif (e>26 && e<=28)
            then System.Drawing.Color.FromArgb(30, 10, 20)
        elif (e>28 && e<=30)
            then System.Drawing.Color.FromArgb(30, 200, 255)
        else
            System.Drawing.Color.FromArgb(0, 0, 0)

(*
 * Тип состояния формы
 *)
type FormState () = 
    let mutable zoom = 550
    let mutable center = Point(0, 0)

    member this.IterationLimit with get() = 50
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