
MaxOfX = 1366
MaxOfY = 768

CenterOfScreenX = 184
CenterOfScreenY = 370
CenterOfSwitchX = 190
CenterOfSwitchY = 678
CenterOfActivityX = 30
CenterOfActivityY = 214

InitialX=0
InitialY=0
FindImageintX=0
FindImageintY=0
FindImageOffsetX = 455
FindImageOffsetY = 765

///////////////////////////////////
ImagePath = "/QMScript/Pic/"
///////////////////////////////////

Function Init()
    '获取当前路径
    MyPath = Plugin.SysEx.GetDir(0)
    ImagePath = MyPath & ImagePath
    Call LocateTiantian()
End Function

'寻找图片
'ImageName  图片名称（此处尽量用中文名字方便理解）
'Retry      重试次数
'rDelay     重试延迟
'Rate       图片匹配率
'如果找到函数返回1，并把图片坐标存储到FindImageintX/Y
Function FindImage(ImageName, Retry, rDelay, Rate)
    FindImage = 0
    While True
        '如果没有定位过，需要全屏查找
        if InitialX=0 and InitialY=0 Then
            FindPic 0, 0, MaxOfX, MaxOfY, ImagePath + ImageName + ".bmp", Rate, FindImageintX, FindImageintY
        Else
            FindPic InitialX,InitialY,InitialX+FindImageOffsetX,InitialY+FindImageOffsetY,ImagePath+ImageName+".bmp", _
            Rate,FindImageintX,FindImageintY
        End if
        If FindImageintX> 0 And FindImageintY> 0 Then
            FindImage = 1
            Exit Function
        End If
        Retry = Retry - 1
        If Retry > 0 Then
            Delay rDelay * 1000
        Else 
            Exit Function
        End If
    Wend
End Function

'定位天天模拟器的位置
function LocateTiantian()
    LocateTiantian = 0
    For 5
        if FindImage("天天模拟器界面",3,0.5,1.0) = 1 Then
            InitialX = FindImageintX
            InitialY = FindImageintY
            MoveTo InitialX, InitialY
            Delay 200
            LocateTiantian = 1
            Exit Function 
        Else 
            MsgBox  "找不到天天模拟器，，，，，"
        End If
    Next 
end function

'拖拽：起点x，y，次数，步数，步长x，y
Function Drag(Cnt, Steps, LenX, LenY)
    For Cnt
        MoveTo InitialX+CenterOfScreenX, InitialY+CenterOfScreenY
        Delay 200
        LeftDown 1
        Delay 500
        For Steps
            MoveR LenX,LenY 
            Delay 20
        Next
        Delay 500
        LeftUp 1
    Next
    MoveTo InitialX+x, InitialY+y
    Delay 500
End Function

'点击+延迟
'OffsetX        坐标偏移
'OffsetY        坐标偏移
'DelayBefore    点击之前延迟
'DelayAfter     点击之后延迟
Function ClickAndDelay(OffsetX, OffsetY, DelayBefore, DelayAfter)
    MoveTo FindImageintX+OffsetX,FindImageintY+OffsetY
    Delay DelayBefore*1000
    LeftClick 1
    Delay 200
    '不遮挡图片
    MoveTo 1,1
    Delay DelayAfter * 1000
end function

'点击+延迟
'OffsetX        坐标
'OffsetY        坐标
'DelayBefore    点击之前延迟
'DelayAfter     点击之后延迟
Function ClickRelative(x, y, DelayBefore, DelayAfter)
    MoveTo InitialX+x,InitialY+y
    Delay DelayBefore * 1000
    LeftClick 1
    Delay 200
    MoveTo 1,1
    Delay DelayAfter * 1000
end function

Function Click(Path)
    'Test:Path = "B52_0.8|349:210|核弹_0.9"
    MyArray = Split(Path, "|")
    CommandCnt = UBound(MyArray)+1
    n = 0
    
    Rem LoopClick
        
    If n >= CommandCnt Then 
        Exit Function
    End If
        
    If InStr(MyArray(n),"_") > 0 Then 
        SArray = Split(MyArray(n), "_")
        If FindImage(SArray(0), 20, 1, CSng(SArray(1))) = 1 Then 
            Call ClickAndDelay(5, 5, 0.3, 0.3)
            n = n + 1
            Goto LoopClick
        End If
    End If

    If InStr(MyArray(n), ":") > 0 Then 
        SArray = Split(MyArray(n), ":")
        Call ClickRelative(CSng(SArray(0)),CSng(SArray(1)),0.3,0.3)
        n = n + 1
        Goto LoopClick
    End If

End Function

Function Exist(Path)
    Exist = 0
    SArray = Split(Path, "_")
    If FindImage(SArray(0), 2, 0.5, CSng(SArray(1))) = 1 Then 
        Exist = 1
    End If
End Function

Function BackToBase()
    While True
        Call Click("模拟器返回键_1.0")
        Delay 500
        If Exist("确定退出游戏_0.9") Then 
            Call Click("模拟器返回键_1.0")
            Exit Function
        End If
    Wend
End Function

Function ClickUntill(Path, Condition)
    ClickUntill = 0
    While True
        Call Click(Path)
        If Exist(Condition) Then 
            ClickUntill = 1
            Exit Function
        End If
    Wend
End Function

Function DragUntill(Cnt,Direction,Path)
    DragUntill = 0
    LenX = 0
    LenY = 0
    While True
        If Exist(Path) Then 
            DragUntill = 1
            Exit Function
        Else 
            If InStr(Direction, "U") Then 
                LenY = -2
            End If
            If InStr(Direction, "D") Then 
                LenY = 2
            End If
            If InStr(Direction, "L") Then 
                LenX = -2
            End If
            If InStr(Direction, "R") Then 
                LenX = 2
            End If
            Call Drag(Cnt, 60, LenX, LenY)
        End If
    Wend
    
End Function

Function WaitUntill(Path)
    While True
        If Exist(Path) Then 
            Exit Function
        Else 
            Delay 500
        End If
    Wend
End Function

Call Init()

'获取航母等级
function GetPirateLevel()
    GetPirateLevel = 0
    GetPirateLevelTemp = 0
    '判断是否是20级海盗
    for 2
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV20-1",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV20-2",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV20-3",1,0,1)
    next
    if GetPirateLevelTemp > 0 Then 
        GetPirateLevel = 20
        exit function
    end if
    '判断是否是21级海盗
    for 2
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV21-1",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV21-2",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV21-3",1,0,1)
    next
    if GetPirateLevelTemp > 0 Then 
        GetPirateLevel = 21
        exit function
    end if
    '判断是否是25级海盗
    for 2
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV25-1",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV25-2",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV25-3",1,0,1)
    Next
    if GetPirateLevelTemp > 0 Then 
        GetPirateLevel = 25
        exit function
    end if
    '判断是否是23级海盗
    for 2
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV23-1",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV23-2",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV23-1",1,0,1)
    Next
    if GetPirateLevelTemp > 0 Then 
        GetPirateLevel = 23
        exit function
    end if
    '判断是否是22级海盗
    for 2
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV22-1",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV22-1",1,0,1)
        GetPirateLevelTemp = GetPirateLevelTemp + FindImage("LV22-1",1,0,1)
    Next
    if GetPirateLevelTemp > 0 Then 
        GetPirateLevel = 22
        exit function
    end if
end Function

Function AttackPirate(Level, Plane, xy, Material, Carrier)
    Call BackToBase()
    Call Click("飞机工厂_0.6")
    Call DragUntill(2, "U", Plane)
    Call Click(Plane & "|" & xy & "|" & Material & "|" & "前往海盗_0.8")
    Delay 3*1000
    Call ClickUntill("184:370", "攻击海盗按钮_0.7")
    If GetPirateLevel() <> Level Then 
        Exit Function
    End If
    Call Click("攻击海盗按钮_0.7")
    Call DragUntill(2,"U",Carrier) '
    Call Click(Carrier & "|派遣航母_0.7")
    Call BackToBase()
    Call Click("码头_0.7|" & Carrier)
    Call WaitUntill("码头确定_0.8")
    'Call BackToBase()
End Function

'Call Click("模拟器返回键_1.0")
'Call BackToBase()
'Call ClickUntill("模拟器返回键_1.0", "确定退出游戏")
'Call DragUntill(2,"U","F-117A轰炸机_0.8")
'Call  Click("184:370")
'MessageBox GetPirateLevel()
'Call AttackPirate(21,"B2轰炸机_0.7","190:240","融合核弹_0.8","黄金加富尔_0.6")
For 40
    Call AttackPirate(22,"女武神_0.7","274:234","爆破弹核心红色_0.8","黄金加富尔_0.6")
next

