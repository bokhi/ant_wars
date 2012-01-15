X = [10;20;30;40;50;100;200;500;1000;2000];
Y = [4.088;8.386;13.544;15.122;20.299;31;66.853;173.485;348.900;721.278];

figure;
plot(X, Y);
xlabel ("population size");
ylabel ("computation time - seconds");
legend ("crossRate 0.7\nmutRate 0.1\n popDepth 2\n popMaxDepth 3\n tournamentSize 3\n expresivennessLevel (10, 10)\n nbMatch 6", "location", "northwest"); 
legend ("right");
title ("influence of the population size on the computation time");
print ("popSize.png");
close;
  
