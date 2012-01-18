data = load ("standardBestAnt.dat");

figure;
hold on;
plot((1:(length (data(:, 1)))), data(:, 1), "0;greedy;");
plot((1:(length (data(:, 1)))), data(:, 2), "1;predator;");
plot((1:(length (data(:, 1)))), data(:, 3), "2;hider;");
plot((1:(length (data(:, 1)))), data(:, 4), "3;wise;");
plot((1:(length (data(:, 1)))), data(:, 5), "4;precautionary;");
xlabel ("generation");
ylabel ("percentage of victory for the genetic ant");
legend("location", "northwest");
legend("right");
title ("evolution of victory percentage of the genetic ant against rule-based ants");
print ("standardBestAnt.png")
close;
