#Functions for mouse control of simulation and plotting (2D and 3D with menus) by using rgl. Written by Hiroshi C. Ito 2020.05.07. ##
# copyright (C) 2020 Hiroshi C. Ito
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as 
# published by the Free Software Foundation.
# http://www.r-project.org/Licenses/

library(rgl)

menu_seal_level=1;
## window size and position
cgl.window_size=600;
cgl.window_pos_x=310;
cgl.window_pos_y=200;
cgl.window_bg_color="gray"

cgl.win_zoom_2d=0.72; ## zoom for par3d function in 2D plot
cgl.pwin_size=600; ## width and height for show2d function in 2D plot
cgl.plot_dim=3; ## plot dimension (2 for 2D and 3 for 3D)

cgl.theta=0.1*pi;
cgl.phi=-0.2*pi

cgl.b0=0;
cgl.b1=NULL;
cgl.nb1=1;

flag_init=1; ## flag for initializaion
flag_pause=1; ## flag for pause
flag_halt=0; ## flag for halt
flag_hide=0; ## flag for hiding menu

flag_show_menu_state=0; ## flag for printing in menu state
color_end="blue"; ## color of "end" button

ncol=100; ## number of colors for each palette
mypal=topo.colors; ## current pallete
col_pal=mypal(ncol); ## color palette of length ncol

## fuction for pan by mouse
## (copied from the example for rgl.setMouseCallbacks)
pan3d <- function(button, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
        start <- list()
        
        begin <- function(x, y) {
          activeSubscene <- par3d("activeSubscene", dev = dev)
          start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene)
          for (sub in start$listeners) {
            init <- par3d(c("userProjection","viewport"), dev = dev, subscene = sub)
            init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
            start[[as.character(sub)]] <<- init
          }
        }
        
        update <- function(x, y) {
            for (sub in start$listeners) {
                init <- start[[as.character(sub)]]
                xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
            mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
            par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
           }
        }
        rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
        cat("Callbacks set on button", button, "of rgl device", dev, "in subscene", subscene, "\n")
}


## fuctions for collecting value from each menu item
mget_state <- function(x){return(x$state);}
mget_label <- function(x){return(x$label[x$state]);}
mget_n <- function(x){return(x$n);}
mget_color <- function(x){return(x$color[x$state]);}



## fuction for collecting values from menu
get_value <- function(x,target,unlis=TRUE){
    if(target=="state")mget_func=mget_state;
    if(target=="n")mget_func=mget_n;
    if(target=="label")mget_func=mget_label;
    if(target=="color")mget_func=mget_color;
    if(unlis==TRUE)return(unname(unlist(lapply(menu,mget_func))));    
    if(unlis==FALSE)return(lapply(menu,mget_func));
    
}


oget_type <- function(x){return(x$type);}

##show existing rgl objects
showobj <- function(obj=scene3d()$objects,type=NULL){
    ob=unlist(lapply(obj,oget_type));
    if(length(type)>0){
        ob=ob[which(ob==type)];
    }
    return(ob);

     
}

##show structure of an rgl object
lookobj <- function (id,obj=scene3d()$objects){
    ob=unlist(lapply(obj,oget_type));
    
    return(str(obj[[which(as.integer(names(obj))==id)]]));

}

##remove rgl objects
remove_obj <- function (obj=scene3d()$objects,type="bboxdeco",id=NULL,nolast=FALSE){
    if(length(id)>0){
        rgl.pop(id=id);
    }
    else{
        ob_type=unlist(lapply(obj,oget_type));
        ob_id=as.integer(names(obj));
        tag_id=ob_id[which(ob_type==type)];
        if(length(tag_id)>0){
            if(nolast==FALSE)rgl.pop(id=tag_id);
            if(nolast==TRUE)rgl.pop(id=tag_id[-which.max(tag_id)]);
            }
    }
}

## function for plotting menu
plot_menu <- function (){
    offset=0.04;
    if(flag_hide==1){
       bgplot3d(plot.new(),bg.color=cgl.window_bg_color);
         
    }
    if(flag_hide==0){
        nmenu=length(menu);
        labs=rep("-------",2*nmenu-1);
        labs[2*seq(nmenu)-1]=get_value(menu,"label");
        cols=rep("black",nmenu+(nmenu-1));
        cols[2*seq(nmenu)-1]=get_value(menu,"color");        
        if((menu_seal_level==2)*(flag_pause==0) > 0){
            cols[2*seq(nmenu)-1]=rep("black",nmenu);
            cols[which(labs=="pause")]="blue";
        }
        nlab=length(labs);

        ##color_end="blue";
        color_hide="blue";
     
        if((menu_seal_level==2)*(flag_pause==0) > 0){
        color_end="black";
        color_hide="black";
        }
        bgplot3d({par(mai=c(0,0,0,0));
            corners = par("usr");par(xpd = TRUE);plot.new();
            text(y = seq(corners[4],corners[3],,nlab), x = rep(corners[2]+offset,nlab), labels=labs,col=cols,srt = 0,pos=2,cex=1.1);
            text(y = c(corners[4],corners[3]), x = rep(corners[1]-offset,2), labels=c("end","hide"),col=c(color_end,color_hide),srt = 0,pos=4,cex=1.1)},
            bg.color=cgl.window_bg_color)


    }
    

}

## action for pause/restart
action_pause <-function(mst){
    flag_pause <<- (flag_pause+1)%%2;
    if(flag_pause==1){


        cat("pause\n");
        Sys.sleep(0.01);
    sob=scene3d()$objects;
    sob_type=unlist(lapply(sob,oget_type));
    sob_id=as.integer(names(sob));

        bg_id=sob_id[which(sob_type=="background")];

##    qu_id=sob_id[which(sob_type=="quads")];
 ##  line_id=sob_id[which(sob_type=="lines")];    
    if(length(bg_id)>1){
        try(rgl.pop(id=bg_id[-which.max(bg_id)]));
    }
  ## if(length(qu_id)>3){
  ##     qu_id=qu_id[-which.max(qu_id)];
  ##      qu_id=qu_id[which(qu_id!=cgl.b0)];
  ##      qu_id=qu_id[which(qu_id!=cgl.b1)];
  ##      rgl.pop(id=qu_id);
  ##  }
   ## if(length(line_id)>3){
   ##     line_id=line_id[-which.max(line_id)];
   ##     line_id=line_id[-which.max(line_id)];
   ##     rgl.pop(id=line_id);
   
  ## }


        }
    if(flag_pause==0)cat("restart\n");

}

## action for switching rotation/pan
action_rotate <- function(mst){
            if(mst==2)pan3d(1);
        
            if(mst==1){
                par3d(mouseMode=c("trackball","zoom","fov","pull") );
                rgl.setMouseCallbacks(button=2, begin=do_mouse_action,dev=rgl.cur(),subscene = currentSubscene3d(rgl.cur()));
                
            }
            
}

##action for plotting in R-window
action_plotR <-function(mst){
    if(menu_seal_level > 0){
        if(cgl.plot_dim==2){
        flag_pause <<- 1;
        menu$pause$state <<- 2;
    }
    }
    devl=dev.list();
    pngid=which(names(devl)=="png")
    if(length(pngid)>0)dev.off(devl[pngid]);

    cgl.plotR();
}


##action for pallete change
action_palette <-function(mst){
    mypal <<- palette_list[[mst]];
   
    col_pal <<- mypal(ncol);
    
    if((flag_pause==1)+(flag_halt==1) > 0)cgl.plot();
}

## action for switching plot dimension (2D/3D)
action_dim <-function(mst){

    cgl.plot_dim <<- mst+1;

    try(remove_obj(type="bboxdeco"));

    if(cgl.plot_dim == 2){
        if((flag_pause==1)+(flag_halt==1) > 0)cgl.plot();                
        view3d(theta=0,phi=0,fov=0,zoom=cgl.win_zoom_2d);
        obj_deco3d <<- decorate3d(xlim=cgl.xlim,ylim=cgl.ylim,zlim=cgl.zlim,box=FALSE,axes=FALSE,aspect=TRUE,xlab=NULL,ylab=NULL,zlab=NULL,col="white");

        
    }
    if(cgl.plot_dim == 3){
        par3d(mouseMode=c("trackball","zoom","fov","pull") );
        rgl.setMouseCallbacks(button=2, begin=do_mouse_action,dev=rgl.cur(),subscene = currentSubscene3d(rgl.cur()));
          
        view3d(theta=0,phi=0,fov=40,zoom=0.8);
        obj_deco3d <<- decorate3d(xlim=cgl.xlim,ylim=cgl.ylim,zlim=cgl.zlim,box=F,axes=T,aspect=TRUE,xlab=cgl.xlab,ylab=cgl.ylab,zlab=cgl.zlab);
        ##par3d(userMatrix=(rotationMatrix(-0.3*pi,1,0,0) %*% rotationMatrix(0.1*pi,0,0,1)))
        par3d(userMatrix=(rotationMatrix(cgl.phi,1,0,0) %*% rotationMatrix(cgl.theta,0,0,1)))
    cgl.plot();
    }

   
    
}

## action for on/off for showing axes
action_axes <-function(mst){

    if(mst==2){
        obnames=names(c(obj_deco3d));
        del_list=which((obnames=="axes") + (obnames=="xlab") + (obnames=="ylab") + (obnames=="zlab")>0);
        if(length(del_list)>0){
            try(rgl.pop(id=(c(obj_deco3d)[del_list])));

        }
    }
    if(mst==1){
        
        obj_deco3d <<- decorate3d(xlim=cgl.xlim,ylim=cgl.ylim,zlim=cgl.zlim,box=F,axes=T,aspect=TRUE,xlab=cgl.xlab,ylab=cgl.ylab,zlab=cgl.zlab);
    }
    
    
}

## get keyboard input and execute it
entercom <- function(){
    coms=readline("enter command: ");
    if(coms!=""){
        print(try(eval(parse(text=coms),envir=.GlobalEnv )));
        entercom();
        }
    else{
        cat("command-mode ended\n");
    }
    
}

## action for enter-command
action_command <-function(mst){
    plot_menu();
    entercom();
    menu$command$state <<- 1;
    if((flag_pause==1)+(flag_halt==1) > 0)cgl.plot();                

}

## menu items
item_pause=list(state=2,n=2,label=c("pause","start"),color=c("blue","red"));
item_rot_pan=list(state=1,n=2,label=c("rotate","pan"),color=c("blue","purple"));
item_plotR=list(state=1,n=1,label=c("plotR"),color=c("blue"));
item_dim=list(state=1,n=2,label=c("2D","3D"),color=c("blue","blue"));
item_axes=list(state=1,n=2,label=c("axes\non","axes\noff"),color=c("blue","blue"));
item_command=list(state=1,n=2,label=c("com-\nmand","com-\nmand"),color=c("blue","red"));

## menu item for palettes
palette_list=c(topo.colors,heat.colors,cm.colors,terrain.colors,rainbow);
palette_list_name=c("topo","heat","cm","terrain","rainbow");
npal=length(palette_list);

item_palette=list(state=1,n=npal,label=paste0("palette\n",palette_list_name),color=rep("blue",npal));

## make menu
menu=list(pause=item_pause,
          rot_pan=item_rot_pan,
          palette=item_palette,
          axes=item_axes,
          plotR=item_plotR,
          dim=item_dim,
          command=item_command);

action=c(action_pause,
         action_rotate,
         action_palette,
         action_axes,
         action_plotR,
         action_dim,
         action_command);

menu$dim$state=cgl.plot_dim-1; ## set initial plotting dimension

nmenu=length(menu); ## number of menu items

## function for action for mouse event (right button click as a defaoult) 
do_mouse_action <-function(mpos_x,mpos_y){
    
    ##cat("x:", mpos_x, "y: ",mpos_y,"\n");
    marx=22.0/600.0;
    mary=22.0/600.0;
    wsize=par3d()$viewport[3:4];

    sedge=seq(marx,1.0-marx,,2*nmenu-1);
    sposx=(mpos_x-marx)/as.double(wsize[1]);
    sposy=(mpos_y-mary)/as.double(wsize[2]);

    if((menu_seal_level < 2)+(menu_seal_level==2)*(flag_pause==1) >0 ){
        if(sposx<0.1){
            if(sposy<0.1){
                flag_halt <<- 1;
            cat("end\n");
                color_end <<- "red";
                plot_menu();
            }
            if(sposy>0.9){
                flag_hide <<- (flag_hide+1)%%2;
                if(flag_hide==1)cat("hide on\n");
                if(flag_hide==0)cat("hide off\n");
                
            }
        }
    }
    
    aid=sum(sposy>sedge[2*seq(nmenu-1)])+1;

    if((menu_seal_level < 2)+(menu_seal_level==2)*((names(menu)[[aid]]=="pause")+(flag_pause==1) )> 0){
        
        if(sposx>0.9){
            menu[[aid]]$state <<- (menu[[aid]]$state)%%(menu[[aid]]$n)+1;
            (action[[aid]])(menu[[aid]]$state);
            
            if(flag_show_menu_state==1){
                cat("menu ",aid, ":", names(menu)[aid],"state", menu[[aid]]$state);
                print(as.matrix(get_value(menu,"state",unlis=FALSE)));
                
            }
        }

        plot_menu();
    }
}


## function for initialization of rgl window and mouse callback functions
cgl.init <- function (window_size= cgl.window_size, window_pos_x= cgl.window_pos_x, window_pos_y=cgl.window_pos_y, window_bg_color=cgl.window_bg_color, win_zoom_2d=cgl.win_zoom_2d,pwin_size=cgl.pwin_size){

    menu$dim$state <<- cgl.plot_dim-1;
    
    cgl.window_size <<- window_size;
    cgl.window_pos_x <<- window_pos_x;
    cgl.window_pos_y <<- window_pos_y;
    cgl.window_bg_color <<- window_bg_color;
    cgl.win_zoom_2d <<- win_zoom_2d;
    cgl.pwin_size <<- pwin_size;
    
    open3d(windowRect=c(cgl.window_pos_x, cgl.window_pos_y, cgl.window_pos_x+cgl.window_size, cgl.window_pos_y+cgl.window_size));

    plot_menu();
    
    rgl.setMouseCallbacks(button=2, begin=do_mouse_action,dev=rgl.cur(),subscene = currentSubscene3d(rgl.cur()));


    if(cgl.plot_dim == 2){
        obj_deco3d <<- decorate3d(xlim=cgl.xlim,ylim=cgl.ylim,zlim=cgl.zlim,box=FALSE,axes=FALSE,aspect=TRUE,xlab=NULL,ylab=NULL,zlab=NULL);
        view3d(theta=0,phi=0,fov=0,zoom=cgl.win_zoom_2d);
    }
    if(cgl.plot_dim == 3){
          ##  par3d(userMatrix=(rotationMatrix(-0.3*pi,1,0,0) %*% rotationMatrix(0.1*pi,0,0,1)))
          ##  obj_deco3d <<- decorate3d(xlim=cgl.xlim,ylim=cgl.ylim,zlim=cgl.zlim,box=F,axes=T,aspect=TRUE,xlab=cgl.xlab,ylab=cgl.ylab,zlab=cgl.zlab);
        view3d(theta=0,phi=0,fov=40,zoom=0.8);
          par3d(userMatrix=(rotationMatrix(cgl.phi,1,0,0) %*% rotationMatrix(cgl.theta,0,0,1)))
          obj_deco3d <<- decorate3d(xlim=cgl.xlim,ylim=cgl.ylim,zlim=cgl.zlim,box=F,axes=T,aspect=TRUE,xlab=cgl.xlab,ylab=cgl.ylab,zlab=cgl.zlab);

    
}

    cat("\nRight button click: menu item\n");
    cat("Left button drag: rotation/pan\n");
    cat("Middle button drag: zoom\n\n");
    flag_init <<- 1;
    
}


## main plotting function
cgl.plot <-function (){
    if(cgl.plot_dim ==3 ){
        cgl.b1 <<- c(cgl.plotRgl(),cgl.b1);

        if(length(cgl.b1)> cgl.nb1){
            try(rgl.pop(id=cgl.b1[(cgl.nb1+1):length(cgl.b1)]));
            cgl.b1 <<- c(cgl.b1[1:cgl.nb1]);
        }

    }
    if(cgl.plot_dim ==2 ){
       
        if(menu_seal_level > 0){
            if(flag_pause==0){
            if(dev.cur()>1){
                graphics.off();
                dev.list();
               }
            }
        }
        cgl.b1 <<- c(show2d(cgl.plotR(),width=cgl.pwin_size,height=cgl.pwin_size),cgl.b1);

        if(length(cgl.b1)> 1){
            try(rgl.pop(id=cgl.b1[(1+1):length(cgl.b1)]));
            cgl.b1 <<- c(cgl.b1[1]);
        }

    }
       
    
}





flag_show_menu_state=0; ##メニュー各項目の状態の表示/非表示





#Demo program for mouse control of simulation and plotting (2D and 3D with menus) by using rgl. Written by Hiroshi C. Ito 2020.05.07. ##
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
