:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_authenticate')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).
:- use_module(library('clpfd')).
:- use_module(library('lists')).
:- use_module('lambda').
:- use_module(library('apply')).
:- use_module('db.pl').
 
:- db_term(isa(_,_)).
:- db_term(colour(_,_)).
:- db_term(parent(_,_)).
:- db_term(level(_,_,_)).
:- db_term(duration(_,_)).
:- db_term(start(_,_)).
:- db_term(end(_,_)).
:- db_term(seq(_,_)).
:- db_term(assigned(_,_)).

main :-
	db_attach('data.pldb',[sync(none)]),
	http_server(http_dispatch, [port(8888)]).

:- http_handler('/', root, []).
:- http_handler('/help', ajaxreply(help), []).
:- http_handler('/people', ajaxreply(people), []).
:- http_handler('/edit', ajaxreply(edit), []).
:- http_handler('/new', new, []).
:- http_handler('/del', del, []).
:- http_handler('/setcolour', setcolour, []).
:- http_handler('/nudgeduration', nudgeduration, []).
:- http_handler('/dropped', dropped, []).

:- http_handler('/skilled', skilled, []).
:- http_handler('/navbar', ajaxreply(navbar), []).

auth(Request, User) :- 
	(   http_authenticate(basic('passwd'), Request, [User|_])
	->  true
	;   throw(http_reply(authorise(basic, 
        'Log in as ad or al with password same as username')))
	).

root(Request) :-
  %	auth(Request, _User),
	http_reply_file('dnd.html',[],Request).

ajaxreply(Builder, Request):-
  call(Builder,H,Request),
  navbar(N,Request),
  phrase(html([N,H]),HTML,[]),
  format('Content-type: text/html~n~n'),
  print_html(HTML).

navbar(H,Request) :-
  %	auth(Request, _User),
  findall(X, isa(X,project), Pros),
  maplist(\P^L^(
    L=td([onclick('ajaxgoto("edit?name='+P+'");')],P)
  ) ,Pros ,ProjLinks),
  append([
    [ td([onclick('ajaxgoto("help")')],'Help'), 
      td([onclick('ajaxgoto("people")')],'People') ],
    ProjLinks,
    [ td([onclick('create("project")')],'Kickoff') ]
  ], TDs),
  H_ = table([class(nav)],[tr(TDs)]),
  H = div([h2('Prontt: the prolog project planner'),H_]).

help(H,Request):-
  H=div([],[
  p('This is a project planner based on constraint logic programming. That means it arranges tasks according to rules and optimisation criteria.'),
  p('Create people and skills on People tab. Click body of table to edit peoples\' skill ratings.'),
  p('Click people to edit them. Click a colour to set colour used in Gantt charts.'),
  p('Use Kickoff to make a new project. A link for the new project appears on the navbar.'),
  p('On the project tab make new tasks. Click a task to edit its properties (just duration for now). '),
  p('Drag Task B onto Task A to say that A must be finished before B can start. Drag A onto B to delete that dependency.'),
  p('Drag a task onto a person to assign it or to nobody to unassign it.'),
  p('Notice how sequential dependencies are observed and nobody gets double booked, even between different projects.'),
  p('For now, optimisation just means minimising the final end date of all tasks.'),
  p('Future features will include:'),
  ul([
    li('Associating multiple skills with tasks'),
    li('Finding people with relevant skills'),
    li('Expecting experts to complete tasks quicker than novices'),
    li('Expecting time to be lost when somebody changes task'),
    li('Prioritising projects for optimisation, e.g. minimise total lateness of all projects.'),
    li('Poaching people from other projects'),
    li('User login and roles of resource, PM of a given project and global admin'),
    li('Cosmetics')
    ])
  ]).

colours(C):- 
  C=[crimson,green,blue,saddlebrown,darkorange,
     darkmagenta,deeppink,teal,grey,coral].

people(H,Request) :-
  %auth(Request, User),
  skilltable(H).

skilltable(H) :-
  findall(X, isa(X,skill), Ss),
  findall(X, isa(X,person), Ps),

  maplist(\X^Y^(Y=th(X)), Ss, Sh),
  append([
    [th('')],
    Sh,
    [th([class('button'),onclick='create("skill")'],'New')]
  ], Toprow),

  maplist( Ss+\P^PR^(
    maplist( P+\S^C^(
      level(P,S,L),C=td([onclick('toggleSkill("'+P+'","'+S+'")')],L)
      ;C=td([onclick('toggleSkill("'+P+'","'+S+'")')],'0')
    ), Ss, PRM),
    colouR(P,PC),
    append([
      [th([class(left),style('color:'+PC),onclick('ajaxgoto("edit?name='+P+'")')],P)],
      PRM,
      [th([class('button'),onclick('destroy("'+P+'")')],'Del')]
    ],PR)
  ) , Ps, PRs),

  maplist(\S^Y^(
    Y=th([class('button'),onclick('destroy("'+S+'")')],'Del')
  ), Ss, Bh),

  append([
    [th([class('left button'),onclick='create("person")'],'New')],
    Bh,
    [th('')]
  ], Botrow),

  append([[Toprow],PRs,[Botrow]], Rows),
  maplist(\R^B^(B=tr(R)),Rows,Body),
  H = table(Body).

new(Request) :-
  %auth(Request, User),
	http_parameters(Request,[ 
    name(I,[]), 
    type(T,[]), 
    parent(P,[optional(true)]) 
  ]),
	( 
		isa(I,_) ; 
		db_assert(isa(I,T)),
    %db_assert(ownedby(I, User)),
    (atom(P),db_assert(parent(I,P));true),
    (T=task,db_assert(duration(I,1));true),
    (T=person,db_assert(colour(I,grey));true)
	),
  ((T=skill),Whereto=people; Whereto=edit_(I)),
	ajaxreply(Whereto,Request).

del(Request) :-
  %auth(Request, _User),
	http_parameters(Request,[name(I,[])]),
	( 
		isa(I,T), 
    (T=task,parent(I,P),Whereto=edit_(P);Whereto=people),
		db_retractall(isa(I,_)), 
		db_retractall(parent(I,_)), 
		db_retractall(colour(I,_)),
		db_retractall(level(I,_,_)),
		db_retractall(level(_,I,_)),
		db_retractall(seq(I,_)), 
		db_retractall(seq(_,I))
		; true
	),
	ajaxreply(Whereto,Request).

setcolour(Request) :-
  %auth(Request, _User),
	http_parameters(Request,[ name(I,[]) , col(C,[]) ]),
	( 
		isa(I,_), 
		db_retractall(colour(I,_)),
		db_assert(colour(I,C))
		; true
	),
	ajaxreply(edit_(I),Request).

nudgeduration(Request) :-
  %auth(Request, _User),
	http_parameters(Request,[ name(I,[]) , change(C,[]) ]),
	( 
		isa(I,task), 
    (duration(I,D);D=1),
    ( C=more,ND is D+1;
      C=less,D>0,ND is D-1;
      ND is D 
    ),
		db_retractall(duration(I,_)),
		db_assert(duration(I,ND))
		; true
	),
	ajaxreply(edit_(I),Request).

edit(H,Request) :-
  %auth(Request, User),
	http_parameters(Request,[ name(I,[]) ]),
  edit_(I,H,Request).

edit_(I, H, Request):-

  ( isa(I,person), 
    colouR(I,PC),
    colours(Cs), maplist(\C^S^(
      S=span([style('color:'+C),onclick('ajaxgoto("setcolour?name='+I+'&col='+C+'")')],C)
    ) ,Cs,Colshow),
    L1 = [p([
      span('This is comrade '),
      span([style('color:'+PC)],I)]),
      div([class('button'),onclick('destroy("'+I+'")')],'Delete Person')],
    append(Colshow, L1, L),
    H = div(L) 
  ); 

	( isa(I,project), 
    findall(X, isa(X,person), People),
    maplist(\P^J^( 
      colouR(P,C), 
      J = tr(td([
        id(P),
        class(left),
        style('color:'+C)
      ],P))) , People, PL_),
    append(PL_,[tr(td([
      id('nobody'),
      class(left)
    ],'nobody'))],PL),
    L_bu = p([div([class('button'),onclick('create("task","'+I+'")')],'New Task'),
      div([class('button'),onclick('destroy("'+I+'")')],'Delete Project')]),
    L_id = p([ span('This is project '), span(I)]), 
    drawgantt(I,SVG),

    Bulk = table(tr([ td([style('vertical-align:top;')],table([],PL)), td([style('vertical-align:top;')],SVG) ])),
    append([ [L_id] , [Bulk], [L_bu] ],Hn),
    H = div(Hn)

  );

	( isa(I,task), 
    L_id = p([ span('This is task '), span(I)]), 
    (duration(I,D);D=1),
    THs = table([tr([
      td('Duration:'),
      td([onclick('ajaxgoto("nudgeduration?name='+I+'&change=less")')],'<'),
      td(D),
      td([onclick('ajaxgoto("nudgeduration?name='+I+'&change=more")')],'>')
    ])]),
    L_bu = p([
      div([class('button'),onclick('destroy("'+I+'")')],'Delete Task')
    ]),
    append([ [L_id] , [THs] , [L_bu] ],Hn),
    H = p(Hn)
  );

  H = p([span('Nothing called '),span(I)]).

drawgantt(I,SVG) :-

    findall(X,(isa(X,task),parent(X,I)),Ts),

    LayoutMargin = 2,
    LayoutNamewidth = 150,
    LayoutDaywidth = 10,
    LayoutLineheight = 23,
    LayoutBarheight = 10,
    LayoutBardown = 13,
    LayoutDown = 16,
    LayoutSpline = 20,
    LayoutTextback = 4,

    length(Ts, NumTasks),
    foldl(\T^O^M^(end(T,TE),(TE>O,M=TE;M=O)), Ts, 0, End),
    GanttWidth is 2*LayoutMargin + LayoutNamewidth + LayoutDaywidth*End,
    GanttHeight is 2*LayoutMargin + LayoutLineheight*NumTasks,
    XZ is LayoutMargin+LayoutNamewidth,
    TR is XZ-LayoutTextback,
    
    SvgLine = [line([ stroke('black'),
      x1(XZ),
      x2(XZ),
      y1(0),
      y2(GanttHeight)
    ],'')],

    maplist(\T^S^(
      nth0(Row,Ts,T),
      colouR(T,C),
      Y is LayoutMargin+Row*LayoutLineheight+LayoutDown,
      S = text([
        fill(C),
        id(T),
        x(TR),
        'text-anchor'(end),
        y(Y),
        class('tasklabel'),
        onmousedown('return mouseDown(this,evt);')
      ],T)), Ts, SvgLabels),

    maplist(\T^S^(
      nth0(Row,Ts,T),
      start(T,Start),
      duration(T,Dur),
      X is LayoutMargin+LayoutNamewidth+LayoutDaywidth*Start,
      Y is LayoutMargin+(Row-1)*LayoutLineheight+LayoutDown+LayoutBardown,
      W is LayoutDaywidth*Dur,
      colouR(T,C),
      S = rect([
        fill(C),
        x(X),
        width(W),
        y(Y),
        height(LayoutBarheight)
      ],'')), Ts, SvgBars),

    findall([S1,S2],(parent(S2,I),parent(S1,I),seq(S1,S2)), Seqs),
    maplist(\[T1,T2]^Spline^(
      nth0(Row1,Ts,T1),
      nth0(Row2,Ts,T2),
      start(T1, S1),
      duration(T1,D1),
      start(T2, S2),
      SY is LayoutMargin+LayoutDown+LayoutBardown+(Row1-1)*LayoutLineheight+LayoutBarheight/2,
      EY is LayoutMargin+LayoutDown+LayoutBardown+(Row2-1)*LayoutLineheight+LayoutBarheight/2,
      SX is LayoutMargin+LayoutNamewidth+LayoutDaywidth*(S1+D1),
      EX is LayoutMargin+LayoutNamewidth+LayoutDaywidth*(S2),
      SC is SX+LayoutSpline,
      EC is EX-LayoutSpline,
      Spline = path([fill('none'),stroke('black'),
        d('M'+SX+','+SY+' C'+SC+','+SY+' '+EC+','+EY+' '+EX+','+EY)],'')
    ), Seqs, SvgSplines),

    append([SvgLine,SvgLabels,SvgBars,SvgSplines],SvgAll),  

    SVG = svg([width(GanttWidth), 
               height(GanttHeight), 
               viewbox('0 0 '+GanttWidth+' '+GanttHeight)],
               SvgAll).


dropped(Request) :-
  %auth(Request, User),
	http_parameters(Request,[ 
    srcid(SI,[optional(true)]), 
    dstid(DI,[optional(true)])
  ]),
  ( 
    (
      isa(SI,task), isa(DI,task),
      % means source after dest, i.e. seq(DI,SI)
      ( DI=SI;
        seq(DI,SI); 
        seq(SI,DI), db_retractall(seq(SI,DI)) ;
        seq_somehow(SI,DI); %dont make circular
        db_assert(seq(DI,SI)) ),
      (parent(DI,P);P=people),
      ajaxreply(edit_(P),Request)
    );
    (
      isa(SI,task), isa(DI,person),
      db_retractall(assigned(SI,_)),
      db_assert(assigned(SI,DI)),
      parent(SI,Pro),
      ajaxreply(edit_(Pro),Request)
    );
    (
      isa(SI,task), DI='nobody',
      db_retractall(assigned(SI,_)),
      parent(SI,Pro),
      ajaxreply(edit_(Pro),Request)
    )
  ;true).

skilled(Request) :-
  %auth(Request, _User),
	http_parameters(Request,[ who(P,[]), what(S,[]) ]),
	( 
		isa(P,_), 
		isa(S,_), 
    (level(P,S,L);L=0),
    (L=3,NL=0;NL is L+1),  
    db_retractall(level(P,S,_)),
    db_assert(level(P,S,NL)) 
		; true
	),
	ajaxreply(people,Request).

%Finally, the logic...

colouR(I,C):- 
  ( isa(I,person),P=I ; isa(I,task),assigned(I,P) ),
  colour(P,C)
  ;C=black.

worstcase(D) :-
  findall(T, isa(T,task), Ts),
  maplist(\T^D^(duration(T,D)), Ts, Ds),
  sumlist(Ds,D).

seq_somehow(A,Z):-
  seq(A,Z);
  seq(Y,Z),
  seq_somehow(A,Y).

end(T,E):-
  start(T,S),
  duration(T,D),
  E is S+D.

start(T,S):-
  schedule(Sched),
  lookup(Sched, T, S).

% Surely this is bult in...
zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).

zip3([], [], [], []).
zip3([W|Ws], [X|Xs], [Y|Ys], [[W,X,Y]|Zs]) :- zip3(Ws,Xs,Ys,Zs).

schedule(Schedule) :-
  findall(T, isa(T,task), Tasks),
  length(Tasks, NumTasks),
  length(Starts, NumTasks),
  maplist(duration, Tasks, Ds), 
  sumlist(Ds,WorstCase),
  Starts ins 0..WorstCase,
  zip(Tasks, Starts, Schedule),

  findall([Pre,Post], seq_somehow(Pre, Post), PPs),
  maplist(inorder(Schedule),PPs),

  findall(Per, isa(Per,person), People),
  maplist(nodoublebookings(Schedule),People),
  
  length(Ends, NumTasks),
  Ends ins 0..WorstCase,
  ends(Schedule, Ends),
  GO in 0..WorstCase,
  maplist(GO+\E^(GO#>=E), Ends),

  append([[GO],Starts,Ends],ToSolve),
  once(labeling([min(GO)],ToSolve)).


inorder(Schedule,[Pre, Post]):-
  duration(Pre, PreDur),
  lookup(Schedule, Pre, PreStart),
  lookup(Schedule, Post, PostStart),
  PostStart #>= PreStart + PreDur.

nodoublebookings(Schedule, Per):-
  include(\[T,S]^(assigned(T,Per)), Schedule, HisSchedule),
  maplist(\[T,S]^S^true, HisSchedule, Sts),
  maplist(\[T,S]^D^(duration(T,D)), HisSchedule, Dus),
  serialized(Sts, Dus).

ends(Schedule, Ends):-
  maplist(\[T,S]^S^true, Schedule, Sts),
  maplist(\[T,S]^D^(duration(T,D)), Schedule, Dus),
  zip3(Sts, Dus, Ends, Threes),
  maplist(\[S,D,E]^(E#=S+D), Threes).

lookup([],_,0).
lookup([[Task, TaskStart]|Es], Task, TaskStart). 
lookup([_|Es], Task, TaskStart) :-  
  lookup(Es, Task, TaskStart).


