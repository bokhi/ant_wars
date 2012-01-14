function pop_stat (file)

  data = load (strcat([file, ".dat"]));

  data(:, (1:2)) = data(:, (1:2)) ./ repmat(data(:, 3), 1, 2);

  figure;
  plot((1:(length (data(:, 1)))), data(:, 1));
  xlabel ("generation");
  ylabel ("average amount of food collected per game");
  title ("evoluation of the average amount of food collected per game per generation");
  print (strcat([file, "_food_stat.png"]));

  close;
  figure;
  plot((1:(length (data(:, 2)))), data(:, 2));
  xlabel ("generation");
  ylabel ("average number of death per game");
  title ("evoluation of the average number of death per game per generation");
  print (strcat([file, "_death_stat.png"]));
  close;

  figure;
  plot((1:(length (data(:, 2)))), data(:, 4));
  xlabel ("generation");
  ylabel ("average expression depth");
  title ("evoluation of the average expression depth per generation");
  print (strcat([file, "_depth_stat.png"]));
  close;

endfunction


