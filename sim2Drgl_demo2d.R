#Mouse control of simulation and 2D plotting by using rgl. Written by Hiroshi C. Ito 2020.05.02. ##

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

dt=0.02; ##時間刻み幅
step=0; ##タイムステップ
zmax=2;##等高線の上限値
zmin=-2;##等高線の下限値

pwin_size=600;##2次元プロットの解像度（ピクセル）

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

##Rデフォルトのウインドウにプロットする関数
mouse_pfunc <-function(mpos_x,mpos_y){
    pfunc(x,x,z);
    cat("plotted in R window\n");
}

##rglのウインドウを生成
open3d(windowRect=c(win_pos_x, win_pos_y, win_pos_x+win_size, win_pos_y+win_size));

##マウス右ボタンのクリックでmouse_pose関数が実行されるようにセット
rgl.setMouseCallbacks(button=2, begin=mouse_pause,dev=rgl.cur(),subscene = currentSubscene3d(rgl.cur()));

##マウス左ボタンのクリックでmouse_pfunc関数が実行されるようにセット
rgl.setMouseCallbacks(button=1, begin=mouse_pfunc,dev=rgl.cur(),subscene = currentSubscene3d(rgl.cur()));

##枠なしの設定
decorate3d(xlim=c(-1,1),ylim=c(-1,1),zlim=c(-2,2),box=FALSE,axes=FALSE,aspect=TRUE,xlab=NULL,ylab=NULL,zlab=NULL);

##視点をｚ軸に沿う下向き方向にセット
view3d(theta=0,phi=0,fov=0,zoom=0.6);

x=seq(-1,1,,64); ##-１〜１までのベクトルを生成
y=x;
xx=matrix(x, nrow=64, ncol=64, byrow=T); ##2次元格子のx座標
yy=matrix(x, nrow=64, ncol=64, byrow=F); ##2次元格子のy座標

##2次元プロット関数
pfunc <-function(x,y,z){
    nlev=17;
    clwd=rep(1.0,17);##等高線の太さを１に設定
    ccol=rep("black",17);##等高線の色を黒にする
    ccol[8]="red";##8番目だけ赤くする
    clwd[8]=3.0;##8番目だけ太さを３にする
    ##塗り潰し等高線(filled.contour)と等高線(contour)を合わせてプロット
    filled.contour(x,x,z,col=topo.colors(20),xlab="x",ylab="y",cex.lab=2.0,levels=seq(zmin,zmax,,nlev),line=1.5,main=paste0("step: ",step),
                   plot.axes = contour(x,x,z, levels =seq(zmin,zmax,,nlev), drawlabels = TRUE, axes = FALSE, frame.plot = FFALSE, add = TRUE,labcex=1.3,col=ccol,lwd=clwd)
                   )
   
}

##メインループ
while(1){
    if(flag_pause==0){##一時停止でないなら以下を実行する
        step=step+1;
        t=dt*step;
        z=sin(2*sqrt(xx^2+yy^2)+t*0.3)+cos(xx+1.3*yy+t);##ｚの更新

        ##pfuncでプロットしたグラフをpng画像にしてx-y平面に張り込んで
        ##そのオブジェクトidをb1に格納
        b1=show2d(pfunc(x,x,z),width=pwin_size,height=pwin_size);

        ##最初以外は古い方のオブジェクトを削除   
        if(step>1)delFromSubscene3d(b0);    
        b0=b1;##b1に格納していたオブジェクトidをb0に格納       
        
    }
    
    Sys.sleep(0.001);##0.001秒待機
    if(flag_halt==1)return(0);## flag_haltが１ならば終了する     
}
