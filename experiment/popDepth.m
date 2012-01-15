X = [1;2;3;4;5;6;7;8;9;10];
Y = [];

figure;
plot(X, Y);
xlabel ("population depth");
ylabel ("computation time - seconds");
legend ("crossRate 0.7\nmutRate 0.1\n popSize 10\n tournamentSize 2\n expresivennessLevel (10, 10)\n nbMatch 6", "location", "northwest"); 
legend ("right");
title ("influence of the population depth on the computation time");
print ("popDepth.png");
close;
  
