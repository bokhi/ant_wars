X = [1;2;3;4;5;6;7;8;9;10;20;30];
Y = [0.288;18.53;34.878;56.765;78.747;88.992;123.331;99.257;101.940;162.417;406.026;509.550];

figure;
plot(X, Y);
xlabel ("tournament size");
ylabel ("computation time - seconds");
legend ("crossRate 0.7\nmutRate 0.1\n popSize 100\n popDepth 2\n popMaxDepth 3\n expresivennessLevel (10, 10)\n nbMatch 6", "location", "northwest"); 
legend ("right");
title ("influence of the tournament size on the computation time");
print ("tournamentSize.png");
close;
  
