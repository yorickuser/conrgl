#Mouse control of simulation and 3D plotting by using rgl. Written by Hiroshi C. Ito 2020.05.02. ##

# copyright (C) 2020 Hiroshi C. Ito
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as 
# published by the Free Software Foundation.
# http://www.r-project.org/Licenses/


library(rgl)  ##rglをロード

win_pos_x=100; ##ウィンドウの生成位置のｘ座標
win_pos_y=200; ##ウィンドウの生成位置のｘ座標
win_size=600; ##ウィンドウサイズ（ピクセル）

flag_pause=0; ##０：一時停止しない, １：一時停止
flag_halt=0; ##１で終了

dt=0.07; ##時間刻み幅
step=0; ##タイムステップ

ncol=50; ##色パレットの色数
col_pal=topo.colors(ncol); ##色パレット
zmax=2; ##色パレットの上限色に対応する値
zmin=-2; ##色パレットの下限色に対応する値

##ｚの値から色を割り当てる関数
calc_col <- function(z,zmin,zmax,ncol,col_pal){
    cv=as.integer(ncol*(z-zmin)/(zmax-zmin));
    cv[which(cv<1)]=1;
    cv[which(cv>ncol)]=ncol;
    return(col_pal[cv]);
}

##マウスクリックで一時停止と終了を制御する関数：
##マウスクリックのｙ座標がウインドウの上部から２０％の範囲にある場合は
##終了(flag_halt=1)
##それ以外の場合は一時停止(flag_pauseを０⇒１ or １⇒０)
mouse_pause <-function(mpos_x,mpos_y){
    if(mpos_y<win_size*0.2){
        flag_halt <<- flag_halt*0+1;
        cat("ended\n");
    }
    else{
        flag_pause <<- (flag_pause+1)%%2;
        cat("pause:",flag_pause,"\n");
    }
}

##rglのウインドウを生成
open3d(windowRect=c(win_pos_x, win_pos_y, win_pos_x+win_size, win_pos_y+win_size));

##マウス右ボタンのクリックでmouse_pose関数が実行されるようにセット
rgl.setMouseCallbacks(button=2, begin=mouse_pause,dev=rgl.cur(),subscene = currentSubscene3d(rgl.cur()));

##枠や視点の向きをセット
decorate3d(xlim=c(-1,1),ylim=c(-1,1),zlim=c(-2,2),box=F,axes=T,aspect=TRUE);
    par3d(userMatrix=(rotationMatrix(-0.3*pi,1,0,0) %*% rotationMatrix(0.1*pi,0,0,1)))


x=seq(-1,1,,64); ##-１〜１までのベクトルを生成
y=x;
xx=matrix(x, nrow=64, ncol=64, byrow=T); ##2次元格子のx座標
yy=matrix(x, nrow=64, ncol=64, byrow=F); ##2次元格子のy座標

##メインループ
while(1){
    if(flag_pause==0){##一時停止でないなら以下を実行する
        step=step+1;
        t=dt*step;
        z=sin(2*sqrt(xx^2+yy^2)+t*0.3)+cos(xx+1.3*yy+t); ##ｚの更新

        ##ｚの値を曲面表示して、そのオブジェクトidをb1に格納
        b1=terrain3d(x,y,z,color=calc_col(z,zmin,zmax,ncol,col_pal));

        ##最初以外は古い方のオブジェクトを削除   
        if(step>1)delFromSubscene3d(b0);
        b0=b1; ##b1に格納していたオブジェクトidをb0に格納       
    }
    
    Sys.sleep(0.001); ##0.001秒待機
    if(flag_halt==1)return(0); ## flag_haltが１ならば終了する     
}
