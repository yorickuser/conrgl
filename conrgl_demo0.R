#Demo program for mouse control of simulation and plotting (2D and 3D with menus) by using rgl. Written by Hiroshi C. Ito 2020.05.06. ##
# copyright (C) 2020 Hiroshi C. Ito
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as 
# published by the Free Software Foundation.
# http://www.r-project.org/Licenses/

library(rgl); ##rglをロード
source("conrgl-0.1.R"); ##conrgl-0.1.Rを読む
 
cgl.plot_dim=3; ##最初に表示する描画の次元
cgl.xlim=c(-1,1); ##ｘ軸の表示範囲
cgl.ylim=c(-1,1); ##ｙ軸の表示範囲
cgl.zlim=c(-2,2); ##ｚ軸の表示範囲
cgl.xlab=NULL; ##ｘ軸のラベル
cgl.ylab=NULL; ##ｙ軸のラベル
cgl.zlab=NULL; ##ｚ軸のラベル

flag_show_menu_state=0; ##メニュー各項目の状態の表示/非表示
dt=0.07; ##時間刻み幅
step=0; ##時間ステップ
nlevs=17; ##等高線の数
zmax=2; ##等高線の最大値
zmin=-2; ##等高線の最小値

 ##等高線を増やすボタンに対応する関数
action_contour_inc <- function(mst){
    nlevs <<- nlevs+1;
    cgl.plot();
}

 ##等高線を減らすボタンに対応する関数
action_contour_dec <- function(mst){
    nlevs <<- max(nlevs-1,1);
    cgl.plot();
}

 ##等高線を増やすボタン要素を作成
item_contour_inc=list(state=1,n=1,label=c("cont+"),color=c("dark green"));
 ##等高線を減らすボタン要素を作成
item_contour_dec=list(state=1,n=1,label=c("cont-"),color=c("dark green"));

 ##メニューに自分用のボタン要素を追加
menu=c(menu,list(contour_inc=item_contour_inc,
                 contour_dec=item_contour_dec));
action=c(action, c(action_contour_inc,
                   action_contour_dec));

nmenu=length(menu); ##メニューの数

##メニュー順序の入れ替え例
menu_order=seq(nmenu);
menu_order=c(1,2,3,4,6,5,7,8);
action=action[menu_order]; 
menu=menu[menu_order]; 

 ##2DプロットとRウインドウでの描画を行う関数
cgl.plotR <- function(){
    clwd=rep(1.0,20);
    ccol=rep("black",20);
    ccol[8]="red";
    clwd[8]=3.0;
    filled.contour(x,x,z,col=mypal(nlevs-1),xlab="x",ylab="y",cex.lab=2.0,levels=seq(zmin,zmax,,nlevs),line=1.5,main=paste0("step: ",step, "  nlevs:",nlevs),
                   plot.axes = contour(x,x,z, levels =seq(zmin,zmax,,nlevs), drawlabels = TRUE, axes = FALSE, frame.plot = FFALSE, add = TRUE,labcex=1.1,col=ccol,lwd=clwd)
                   )
   
}

 ##3Dプロットを行う関数（描画関数の戻り値を返すように設定する!）
cgl.plotRgl <- function(){
                cv=as.integer(ncol*(z-zmin)/(zmax-zmin));
                cv[which(cv < 1)]=1;
                cv[which(cv > ncol)]=ncol;
               
                return(terrain3d(x,x,z,color=col_pal[cv]));
}

cgl.init(); ##初期化のための関数

t=0.0; ##時刻を０にする
x=seq(-1,1,,64); ##-１〜１までのベクトルを生成
xx=matrix(x, nrow=64, ncol=64, byrow=T); ##2次元格子のｘ座標
yy=matrix(x, nrow=64, ncol=64, byrow=F); ##2次元格子のｙ座標
z=sin(2*sqrt(xx^2+yy^2)+t*0.3)+cos(xx+1.3*yy+t); ##各位置のｚの値を計算

cgl.plot(); ##初期状態の描画

while(1){
    if(flag_pause==0){##一時停止でないなら以下を実行する
        step=step+1;
        t=dt*step;
        ##ｚの更新（ここにシミュレーション１ステップの実行関数をセットすればいい）	
        z=sin(2*sqrt(xx^2+yy^2)+t*0.3)+cos(xx+1.3*yy+t);
        cgl.plot(); ##描画
        if(step%%10==0)cat("step: ",step,"\n");
 
    }
    
    Sys.sleep(0.001); ##0.001秒待機
    if(flag_halt==1)return(0); ## flag_haltが１ならば終了する    
}

