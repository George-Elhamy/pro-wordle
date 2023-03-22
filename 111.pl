


build_kb:-
		write('Please enter a word and its category on separate lines:'),nl,
		read(Word),
		(Word=done,nl,
		write('Done building the words database...'),nl,nl;
		read(Category),
		assert(word(Word,Category)),build_kb).
				
is_category(C):-word(_,C).

categories(L):-
		setof(C,is_category(C),L).
					
available_length(L):-
		word(X,_),
		string_chars(X,Z),
		length(Z,L),!.
		
pick_word(W,L,C):-
		word(W,C),
		string_chars(W,X),
		length(X,L).

correct_letters([],_,[]):- !.
correct_letters(L1,L2,L):-
		setof(P,helper(L1,L2,P),L).
helper(L1,L2,P):-
		member(P,L1),member(P,L2).
		

correct_positions([],_,[]):- !.
correct_positions([H1|T1],[H1|T2],[H1|T3]):-
		correct_positions(T1,T2,T3).
correct_positions([H1|T1],[H2|T2],L3):- 	
		H1\=H2,
		correct_positions(T1,T2,L3).


play:-
		write('The available categories are: '),categories(L),write(L),nl,
		play1(X),
		play2(X,Z),
		pick_word(Z,Lg,X),L1 is Lg+1,write('Game started. You have '),write(L1),write(' guesses.'),nl,
		play3(Z,Lg,L1).



play1(X):-
		write('Choose a category: '),nl,
		read(Y),
		(is_category(Y),X=Y;
		write('This category does not exist.'),nl,play1(X)).
	 
play2(X,Z):-
		write('Choose a length: '),nl,
		read(Y),
		(pick_word(Z,Y,X);
		write('There are no words of this length.'),nl,play2(X,Z)).

play3(Z,L,Acc):-
		Acc>1,nl,write('Enter a word composed of '),write(L),write(' Letters'),nl,
		read(X),
		
		(X=Z,write('You Won!');
		
		string_chars(X,Xlist),length(Xlist,L),string_chars(Z,Zlist),
		correct_letters(Xlist,Zlist,Result),write('Correct letters are: '),write(Result),nl,
		correct_positions(Xlist,Zlist,Result2),write('Correct letters in correct positions are: '),
		write(Result2),nl,write('Remaining Gusses are : '),Acc2 is Acc-1,write(Acc2),nl,play3(Z,L,Acc2);
		
		string_chars(X,Xlist),length(Xlist,L),string_chars(Z,Zlist),
		\+correct_letters(Xlist,Zlist,Result),write('Correct letters are: []'),nl,
		write('Correct letters in correct positions are: []'),nl,
		write('Remaining Gusses are : '),Acc2 is Acc-1,write(Acc2),nl,play3(Z,L,Acc2);
		
		string_chars(X,Xlist),\+length(Xlist,L),
		write('Word is not composed of 5 letters. Try again.'),nl,
		write('Remaining Gusses are : '),write(Acc),nl,play3(Z,L,Acc)).

play3(Z,L,Acc):-
		Acc=1,nl,write('Enter a word composed of '),write(L),write(' Letters'),nl,
		read(X),
		(X=Z,write('You Won!');
		X\=Z,write('You Lost!')).

main:-
		write('Welcome to Pro-Wordle!'),nl,
		write('--------------------------------'),nl,nl,
		build_kb,
		play.