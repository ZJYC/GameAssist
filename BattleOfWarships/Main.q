
ImagePath="C:\Users\ZJYC\Desktop\BattleOfWarships\Picture\"

Function Init()
    InitialX=0
    InitialY=0
    FindImageintX=0
    FindImageintY=0
    AllShipsOut=0
    SearchX1 = 122
    SearchY1 = 269
    SearchX2 = 329
    SearchY2 = 462
    '每移动28个像素为一海里
    Distance2Steps = 28
    '我们走一步移动几个海里
    LenOfStepUD = (SearchY2 - SearchY1) / Distance2Steps
    LenOfStepLR = (SearchX2 - SearchX1) / Distance2Steps
    'MsgBox "LenOfStepUD" & LenOfStepUD
    'MsgBox "LenOfStepLR" & LenOfStepLR
    '我们基地的坐标
    OurBaseX = 524  
    OurBaseY = 1100 
    '搜索区域的中心点
    SearchAreaX = 493
    SearchAreaY = 1241
    SearchAreaR = 100
End Function

TimePlanFirstRun = 0

Call SynthesisTask()

Function FindImage(ImageName,Retry,rDelay,Rate)
    FindImage = 0
    for Retry
        if InitialX=0 and InitialY=0 Then
            FindPic 0,0,1366,768,ImagePath+ImageName+".bmp",Rate,FindImageintX,FindImageintY
        Else
            FindPic InitialX,InitialY,InitialX+455,InitialY+765,ImagePath+ImageName+".bmp", _
            Rate,FindImageintX,FindImageintY
        End if
        If FindImageintX> 0 And FindImageintY> 0 Then
            FindImage = 1
            Exit for
        End if
        Delay rDelay*1000
    Next
End Function

Function CloseGame()
    While 1
        If FindImage("返回键", 1, 0, 0.9) = 1 Then 
            Call ClickAndDelay(15, 2, 2)
            If FindImage("确定退出游戏", 1, 0, 0.9) = 1 Then 
                Call ClickAndDelay(10, 2, 2)
                Exit Function
            End If
        End If
        Delay 1000
    Wend
End Function

function LocateTiantian()
    LocateTiantian=0
    if FindImage("TianTian2",4,2,1.0) = 1 Then
        InitialX = FindImageintX
        InitialY = FindImageintY
        SearchX1 = SearchX1 - InitialX
        SearchX2 = SearchX2 - InitialX
        SearchY1 = SearchY1 - InitialY
        SearchY2 = SearchY2 - InitialY
        LocateTiantian=1
    else
        massagebox "Can not find Tiantian...."
    end if
end function

function IsGameRuning()
    IsGameRuning=0
    if FindImage("Run1",1,0,0.8)=1 Or FindImage("Run2",1,0,0.8)=1 Then
        IsGameRuning=1
    End if
End function

function ClickAndDelay(Offset,DelayBefore,DelayAfter)
    MoveTo FindImageintX+Offset,FindImageintY+Offset
    Delay DelayBefore*1000
    LeftClick 1
    Delay DelayAfter*1000
end function

function EnsureGameRuning()
    EnsureGameRuning=0
    if IsGameRuning()=0 Then
        if FindImage("GameIcon",2,2,0.6)=1 Then
            Call ClickAndDelay(15, 2, 2 * 60)
        Else 
            Call FindImage("返回键", 2, 2, 0.9)
            call ClickAndDelay(20,2,2)
        end if
    Else 
        EnsureGameRuning=1
    End if
end function

function EnsureOutOfBase()
    EnsureOutOfBase=0
    if FindImage("Run2",1,0,0.8)=1 Then 
        EnsureOutOfBase=1
    End if
    if EnsureOutOfBase=0 Then
        call FindImage("Run1",1,0,0.8)
        call ClickAndDelay(15,2,4)
    end if
end function

Function EnsureInBase()
    EnsureInBase = 0
    if FindImage("Run1",1,0,0.8)=1 Then 
        EnsureInBase=1
    End if
    if EnsureInBase=0 Then
        call FindImage("Run2",1,0,0.8)
        call ClickAndDelay(15,2,4)
    end if
End Function

function SelectPirates(Cnt)
    SelectPirates=0
    if FindImage("1",1,0,0.6)=1 Then
        call ClickAndDelay(10,2,7)
    end if
    
    if Cnt MOD 3=0 Then 
        if FindImage("东海海盗",4,1,0.7)=0 Then
            exit function
        end if
    End if
    if Cnt MOD 3=1 Then 
        if FindImage("地中海海盗",4,1,0.7)=0 then
            exit function
        end if
    End if
    if Cnt MOD 3=2 Then 
        if FindImage("索马里海盗",4,1,0.7)=0 then
            exit function
        end if
    End if

    Call ClickAndDelay(30, 2, 7)
    '游戏屏幕中间位置
    MoveTo InitialX+238,InitialY+370
    Delay 1000
    LeftClick 1
    Delay 1000
    
    if FindImage("LV20-1",1,0,0.9)+FindImage("LV20-2",1,0,0.9)+FindImage("LV20-3",1,0,0.9)>0 Then
        if FindImage("攻击",1,0,0.9)=1 Then 
            call ClickAndDelay(10,2,2)
            SelectPirates=1
        end if
    end if
end function

function WaitCarrierBack(WaitFor)
    WaitCarrierBack=0
    Call ShowCarrier()
    Delay 2000
    If FindImage("驻防中", WaitFor, 1, 0.7) = 1 Then 
        Call ClickAndDelay(0,1,2)
        if FindImage("派遣",2,1,0.7)=1 Then
            call ClickAndDelay(10,1,3)
            WaitCarrierBack=1
        end if
    end if
end function

function MainTask()
    Call Init()
    cnt = 0
    NoLv21 = 0
    for 10000
        cnt = cnt + 1
        If cnt >= 25 Then 
            Exit for
        End If
        rem Restart
        if LocateTiantian()=0 Then
            goto Restart
        end if
        if EnsureGameRuning()=0 Then 
            goto Restart
        End If
        
        If AttackLv21Pirate() = 0 Then 
            If AttackLv20Pirate(cnt) = 0 Then 
                    goto Restart
            End If
        End If
        'MsgBox cnt
    next
end function

function StepUDs(n)
    For Abs(n)
        If AllShipsOut = 1 Then 
            Exit Function
        End If
        if n > 0 Then 
            call StepDown()
        End if
        if n < 0 Then 
            call StepUp()
        End If
        Call SearchAndDiscovery()
    next
end function

function StepLRs(n)
    For Abs(n)
        If AllShipsOut = 1 Then 
            Exit Function
        End If
        if n > 0 Then 
            call StepRight()
        end if
        if n < 0 Then 
            call StepLeft()
        End If
        Call SearchAndDiscovery()
    next
end function

'所要到达点的坐标
function GotoSatrtPoint(PointX,PointY)
    '相对坐标
    RePointX = PointX - OurBaseX
    RePointY = PointY - OurBaseY
    '相对要移动的次数
    StepsUD = RePointY / LenOfStepUD
    StepsLR = RePointX / LenOfStepLR
    'MsgBox "UD" & StepsUD
    'MsgBox "LR" & StepsLR
    
    StepUDs(StepsUD)
    StepLRs(StepsLR)
    delay 1000
end function

function CircleOneTime(n)
    i=0
    for 4
        For 2 * n
            If AllShipsOut = 1 Then 
                Exit Function
            End If
            if i MOD 4 = 0 Then 
                call StepUDs(1)
            end if
            if i MOD 4 = 1 Then 
                call StepLRs(-1)
            end if
            if i MOD 4 = 2 Then 
                call StepUDs(-1)
            end if
            if i MOD 4 = 3 Then 
                call StepLRs(1)
            end if
        next
    i=i+1
    next
end function

Function SubTask()
    i = 0
    AllShipsOut = 0
    Call Init()
    Call LocateTiantian()
    call FocusOnBase()
    Call GotoSatrtPoint(SearchAreaX, SearchAreaY)
    'MsgBox "GotoSatrtPoint Finished..."
    For SearchAreaR
        if AllShipsOut=1 Then
            Exit Function
        end if
        i=i+1
        StepUDs(-1)
        StepLRs (1)
        'MsgBox "Ready to Discovery..."
        call CircleOneTime(i)
    next
end function

function FocusOnBase()
    if FindImage("Run2",1,0,0.8)=1 Then 
        call ClickAndDelay(15,3,3)
        call ClickAndDelay(15,3,3)
    else
        call FindImage("Run1",1,0,0.8)
        call ClickAndDelay(15,3,3)
    end if
    MoveTo InitialX+235,InitialY+370
    delay 1000
    KeyDown "Ctrl", 1
    delay 1000
    for 10
        MouseWheel -1
        delay 1000
    next
    KeyUp "Ctrl", 1
end function

Function SearchAndDiscovery()
    if FindTile()=0 Then
        exit function
    end if
    call ShowCarrier2()
    Call FindCarrierToDiscovery()
end function

function FindCarrierToDiscovery()
    i = 0
    For 3
        If FindImage("待命中", 2, 2, 0.7) = 1 Then 
            i = i + 1
            Call ClickAndDelay(0, 2, 2)
            Exit For
        End if
    Next
    If i > 0 Then 
        AllShipsOut = 0
    Else 
        AllShipsOut = 1
    End If

    if AllShipsOut=0 Then
        call FindImage("派遣采矿船",2,2,0.8)
        call ClickAndDelay(10,2,2)
    end if
end function

Function FindTile()
    Delay 2000
    FindTile = 0
    For 6
        FindPic InitialX+52,InitialY+214,InitialX+414,InitialY+540,ImagePath+"核反应堆13"+".bmp",0.3,FindImageintX,FindImageintY
        'FindPic 124, 161, 393, 564, "C:\Users\Zhaojingyangchun\Desktop\游戏辅助\2\核反应堆13.bmp", 0.3, FindImageintX, FindImageintY
        If FindImageintX > 0 and FindImageintY > 0 Then 
            FindImageintY = FindImageintY + 15
            call ClickAndDelay(5,2,2)
            FindPic InitialX+140,InitialY+210,InitialX+390,InitialY+510,ImagePath+"勘探"+".bmp", _
                0.5,FindImageintX,FindImageintY
            if FindImageintX>0 and FindImageintY>0 Then 
                call ClickAndDelay(5,2,2)
                FindTile = 1
                Exit For
            Else 
                Call FindImage("关闭海盗选框", 2, 2, 0.8)
                if FindImageintX>0 and FindImageintY>0 Then 
                    Call ClickAndDelay(3, 2, 2)
                End If
                Exit For
            end if
        End If
        Delay 500
    next
end function

'拖拽：起点x，y，次数，步数，步长x，y
Function Drag(x, y, Cnt, Steps, LenX, LenY)
    For Cnt
        MoveTo InitialX+x, InitialY+y
        Delay 500
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
    Delay 1000
End Function
'将航母列表拉到头
function ShowCarrier()
    call Drag(260,375,5,150,0,2)
end function
'展示3/4/5号采矿船
function ShowCarrier2()
    call ShowCarrier()
    call Drag(260,375,1,200,0,-2)
end function
'将视野向右边移动
function StepRight()
    Call Drag(SearchX2,SearchY2,1,SearchX2-SearchX1,-1,0)
end function
'将视野向左边移动
function StepLeft()
    Call Drag(SearchX1,SearchY2,1,SearchX2-SearchX1,1,0)
end function
'将视野向上移动
function StepUp()
    Call Drag(SearchX2,SearchY1,1,(SearchY2-SearchY1)*19/28,0,1)
end function
'将视野向下移动
function StepDown()
    Call Drag(SearchX2,SearchY2,1,(SearchY2-SearchY1)*19/28,0,-1)
end function

'返回为1则说明活动开始了0|2->3|5->6|8->9|
Function TimePlan_()
    '紧急海盗活动：2-3，10-11，18-19
    TimePlan_ = 0
    i = 0   
    Dim Plan
    Plan = Array(2,3,10,11,18,19)
    MyTime = Now
    MyHour = Hour(MyTime)
    For 3
        If MyHour >= Plan(i) and MyHour < Plan(i + 1) Then 
            TimePlan_ = 1
            Exit Function
        End If
        i = i + 2
    Next
End Function

'参数为等待时长，单位小时
Function TimePlan(Len)
    Counter = Len * 60'分钟
    if Counter <= 0 Then
        MsgBox "请给个合理的等待时长然后重启脚本"
    end if
    '第一次运行直接打海盗
    if TimePlanFirstRun = 1 Then 
        TimePlanFirstRun = 0
        exit function
    end if
    for Counter
        '海盗活动事件开始，自动启动打海盗
        If TimePlan_() = 1 Then 
            Delay 10 * 60 * 1000
            exit function
        end if
        '没有海盗活动的话，延迟满Len（一般是2）个小时退出
        delay 60*1000
    next
end function
'混合任务

Function SynthesisTask()
    For 100
        Call TimePlan(2.0)
        Call MainTask()
        'Call SubTask()
        Call CloseGame()
    Next
End Function

Function AttackLv21Pirate()
    AttackLv21Pirate = 0
    i = 0
    '确保我们在基地内部
    Call EnsureInBase()
    '找到飞机工厂 & 点击进去
    If FindImage("飞机工厂", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(25, 2, 4)
        i = i + 1
    End If
    '找到鸭嘴兽轰炸机 & 点进去 
    If FindImage("鸭嘴兽轰炸机", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(35, 2, 4)
        i = i + 1
    End If
    '找到飞机升级按钮 & 点进去 
    If FindImage("飞机升级按钮", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(10, 2, 4)
        i = i + 1
    End If
    '找到融合核弹 & 点进去 
    If FindImage("融合核弹", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(10, 2, 4)
        i = i + 1
    End If
    '前往海盗
    If FindImage("升级飞机前往海盗", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(10, 2, 6)
        i = i + 1
    End If  
    '点击屏幕中间的海盗显示海盗信息
    If i = 5 Then 
        MoveTo InitialX+238,InitialY+370
        Delay 1000
        LeftClick 1
        Delay 1000
    Else 
        Call FindImage("返回键", 2, 2, 0.9)
        Call ClickAndDelay(20, 2, 2)
    End If
    '确保海盗的等级为21
    If FindImage("LV21-索马里", 2, 1, 0.9) = 1 Then 
        AttackLv21Pirate = 1
    End If
    If AttackLv21Pirate = 1 Then 
        '点击攻击
        If FindImage("攻击", 1, 0, 0.9) = 1 Then 
            Call ClickAndDelay(5,1,2)
        End If
        
        Call ShowCarrier()
        Delay 2000
        If FindImage("驻防中", 600, 1, 0.7) = 1 Then 
            AttackLv21Pirate = 0
            Call ClickAndDelay(0,1,2)
            If FindImage("自动填充战舰", 2, 1, 0.7) = 1 Then 
                Call ClickAndDelay(10, 1, 3)
            End If
            If FindImage("战舰填充完成", 3, 2, 1.0) = 1 Then 
                If FindImage("派遣", 2, 1, 0.7) = 1 Then 
                    'MsgBox "Ready....."
                    Call ClickAndDelay(10, 1, 3)
                    AttackLv21Pirate = 1
                    Call EnsureInBase()
                    Call RepairAllShips()
                End If
            End If
        End if
        
    End If
    
End Function

Function RepairAllShips()
    RepairAllShips = 0
    i = 0
    Call EnsureInBase()
     
    If FindImage("码头", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(25, 2, 4)
        i = i + 1
    End If
    
    If FindImage("维修厂", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(5, 2, 4)
        i = i + 1
    End If

    If FindImage("维修当前队列", 300, 1, 0.8) = 1 Then 
        Call ClickAndDelay(5, 2, 4)
        i = i + 1
    End If

    If FindImage("维修", 2, 1, 0.8) = 1 Then 
        Call ClickAndDelay(5, 2, 4)
        i = i + 1
    End If

    If FindImage("维修完成", 300, 1, 0.8) = 1 Then 
        Call FindImage("返回键", 2, 2, 0.9)
        Call ClickAndDelay(20, 2, 2)
        i = i + 1
    End If
    
    If i = 5 Then 
        RepairAllShips = 1
    End If

End Function

Function AttackLv20Pirate(i)
    AttackLv20Pirate = 0
    For 10
        rem R1
        if EnsureOutOfBase()=0 Then
            goto R1
        End If
        if SelectPirates(i)=0 Then
            goto R1
        end if
        if WaitCarrierBack(6*60)=0 Then
            Goto R1
        Else 
            AttackLv20Pirate = 1
            Exit Function
        End If
    Next
End Function



