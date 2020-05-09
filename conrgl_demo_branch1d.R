#Demo program for mouse control of simulation and plotting (2D and 3D with menus) by using rgl. Written by Hiroshi C. Ito 2020.05.09. ##
# copyright (C) 2020 Hiroshi C. Ito
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as 
# published by the Free Software Foundation.
# http://www.r-project.org/Licenses/

##library(rgl); ##rglをロード
##source("conrgl-0.12.R"); ##conrgl-0.12.Rを読む

K0=10.0; ##環境収容力の最大値
sK=1.0; ##環境収容力の幅
sa=0.5; ##競争効果の幅
##sa=0.7; 
##sa=1.2;
ndiv=64; ##形質軸の刻み幅
edge_die=1e-4; ##最小個体数密度。これ以下は絶滅。
repeat_p=1000; ##変異体を出現させずに個体数動態をまわすステップ数（速く計算するため）
dt=0.2; ##時間刻み幅
mutation_rate=3.0e-4; ##突然変異率

time_end=150000;##終了時刻
t_end=as.integer(time_end/(dt*repeat_p)); ##総タイムステップ数
set.seed(23); ##乱数の種。これをコメントアウトすれば毎回少し違う進化動態になる

xx=seq(-1,1,length=ndiv);  ##とり得る形質値のセット
x=xx;
n=xx*0.0;  ##形質軸上の個体数分布
ancestor=c(ndiv/8); ##初期の表現型を指定。
flag_quick=T;
out_interval=5;
show_interval=1;
wins=500;

amp_n=1000;
zcol_max=log(amp_n*K0); ##等高線の最大値
zcol_min=log(amp_n); ##等高線の最小値
cgl.plot_dim=3; ##最初に表示する描画の次元
cgl.xlim=c(-1,1); ##ｘ軸の表示範囲
cgl.ylim=c(0,time_end); ##ｙ軸の表示範囲
##cgl.zlim=c(0,K0*2); ##ｚ軸の表示範囲
cgl.zlim=c(-1,1); ##ｚ軸の表示範囲
cgl.xlab="Trait x (niche position)"; ##ｘ軸のラベル
cgl.ylab="Time"; ##ｙ軸のラベル
cgl.zlab=NULL; ##ｚ軸のラベル
cgl.phi=-0.1*pi;
cgl.theta=0.0*pi

nlevs=20; ##等高線の数（被食者）
nlevs2=7; ##等高線の数（捕食者）

Rxlab="Trate x"; ##x軸のラベル
Rylab="Rime"; ##y軸のラベル


 ##2DプロットとRウインドウでの描画を行う関数
cgl.plotR <- function(){
    zcol=log(n_out+edge_die);

    image(xx,t_out,zcol,useRaster=F,col=mypal(nlevs),xlab="Trait x",ylab="Time",cex.lab=1.3,main=paste0("Time: ",t_out[out_count]));
    
    lines(x=xx,y=time_end*0.5*land_out[,out_count]+1.0,col="cyan",lwd=2);
    lines(x=xx,y=time_end*0.01*log((n+edge_die)/edge_die)+1.0,col="red",lwd=2);
    
}

##3Dプロットを行う関数（描画関数の戻り値を返すように設定する!）
cgl.nb1=2;##オブジェクトの数
cgl.plotRgl <- function(){
    idd=out_count;
    if(idd<5)idd=5;
    
    ##zcol=log(amp_n*(n_out[,1:idd]+1));
    zcol=land_out;
    zcol_min=-1;
    zcol_max=1;
    cv=as.integer(ncol*(zcol-zcol_min)/(zcol_max-zcol_min));
    cv[which(cv < 1)]=1;
    cv[which(cv > ncol)]=ncol;
    cols=col_pal[cv];
    cols[which(n_out[,1:idd]>=edge_die)]="#FF5500";
    bid=terrain3d(xx,t_out[1:idd],(0.02*n_out+land_out)[,1:idd],color=cols);
    material3d(alpha=c(0.5));
    bid=c(bid,terrain3d(xx,t_out,0*land_out,color="blue"));
    material3d(alpha=c(1.0));
        return(bid);
}

 ##環境収容力の分布
K <- function(x){
    return (K0*exp(-x^2/(2.0*sK^2)));
}

 ##競争効果の関数
alpha <- function (x0,x1){
    return (exp(-(x0-x1)^2/(2.0*sa^2)));
}

 ##変異型x1の適応度
fitness <-function(x1,xx,n){
    return(1.0-sum(alpha(xx,x1)*n)/K(x1));
}

 ##突然変異の関数
mutate <- function(xx,n){
    for(i in 1:length(xx)){
        if(rbinom(1,size=1,prob=mutation_rate*n[i]*repeat_p*dt)>0){
            if(rbinom(1,size=1,prob=0.5)>0){
                if((i < ndiv))n[i+1]=n[i+1]+edge_die*2.0;
            }else{
                if((i > 1))n[i-1]=n[i-1]+edge_die*2.0;
            }
        }
    }
    return(n);
}

n[ancestor]=K(xx[ancestor])*0.5;  ##ancestorの位置に０でない個体数をセット
n_out=matrix(ndiv*(t_end/out_interval+1),nrow=ndiv,ncol=(t_end/out_interval+1))*0.0; ##出力用行列（個体数分布の時系列データ）
land_out=n_out*0.0;##適応度地形を格納する行列
##t_out=dt*(0:(t_end/out_interval)); ##出力用の時刻データ
t_out=dt*out_interval*repeat_p*(0:(t_end/out_interval)); ##出力用の時刻データ
time=0.0;  ##時刻
n_out[,1]=n;  ##出力用行列に初期状態を格納
t_exec_start=proc.time();
out_count=1;
##初期設定
cgl.init();
##プロットしておく
cgl.plot();
t=0;
while(1){
    if(t  == t_end+1)flag_halt <<- 1;
    if(flag_halt==1){
                return(0);
}
    if(flag_pause ==0){
        t = t+1;
        n=mutate(xx,n);  ##突然変異   
        mask_n=(n>edge_die);  ##存在する（最小個体数密度を上回る）表現型に目印をつける
        xxb=xx[mask_n];  ##存在する表現型だけにする（計算を速くするため）
        nb=n[mask_n];  ##存在する表現型だけの個体数密度
        dnb=nb*0.0;
        fit=dnb*0.0;

        if(flag_quick==TRUE){## exp関数を呼ぶ回数を減らす小細工
            len_nb=length(nb);
            alphab=matrix(len_nb*len_nb,nrow=len_nb,ncol=len_nb);
            for(i in 1:len_nb){
                for(j in 1:len_nb)alphab[i,j]=alpha(xxb[i],xxb[j]);
            }
            Kb=K(xxb);
        }
        
        for(k in 1:repeat_p){            
            if(flag_quick==TRUE){
                fit=(1.0-colSums(alphab*nb)/Kb); ##適応度の計算
                nb=nb+dt*nb*fit;  ##個体数密度を変化させる
            }
            else{
                for(i in 1:length(fit))fit[i]=fitness(xxb[i],xxb,nb);  ##適応度の計算 
                dnb=nb*fit;  ##増加率の計算
                nb=nb+dt*dnb; ##個体数密度を変化させる
            }
            
            time=time+dt;  ##時刻を更新
        }
        n[mask_n]=nb;  ##全ての表現型の個体数のベクトルに戻す
        n[which(n<=edge_die)]=0.0;  ##edge_die以下の表現型は絶滅
        
                
        if(t%%out_interval==0){
            n_out[,out_count]=n;  ##出力用行列に格納
            out_count=out_count+1;
            cat("  time:", time,"\n");

            buf=rep(0,ndiv);
            for(i in 1:length(land_out[,1])) buf[i] = fitness(xx[i],xxb,nb);
            land_out[,out_count]=buf;
            
            if(out_count%%show_interval==0){

                cgl.plot();
                
            }
        }

    }
    Sys.sleep(0.001);
}


cat("calculation time:\n");
print(proc.time()-t_exec_start);
