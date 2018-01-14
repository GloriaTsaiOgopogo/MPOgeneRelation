import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Date;

public class DistanceCalculator2 {
	
	public static void main(String[] args) throws Exception{
		String path = ""; //args[0];
		String infile = ""; //args[1];
		double maxDistance = Double.valueOf(100.0);
				
		Date begin = new Date();
		System.out.println("Begin Time: " + begin);
		
		distanceMatrix(path, infile, maxDistance);

		Date end = new Date();
		System.out.println("End Time: " + end);
	}
	// By Qingqing Cai 17110700093  
	public static void distanceMatrix(String bootFilePath, String filename, double maxDistance) throws Exception{
		BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(bootFilePath + filename )));
		PrintWriter wr = new PrintWriter(new BufferedWriter(new FileWriter(bootFilePath + "MPO_dist_table.txt")));
		PrintWriter wr2 = new PrintWriter(new BufferedWriter(new FileWriter(bootFilePath + "SuperMPO.txt"))); 
		//PrintWriter wr3 = new PrintWriter(new BufferedWriter(new FileWriter(bootFilePath + "independent_MPO.txt")));   //太多了
			
		String line = null;
		
		int count=0;
		
		while((line = br.readLine()) != null){
			if(line.contains("MPO") == false){
				int row_num = 0;
				int col_num = 0;	
				String[] MPO = null;
				int[][] matrix = null;
				
				String num_format = "(\\d+)";
				
				Pattern pt_num = Pattern.compile(num_format);	
				Matcher match = pt_num.matcher(line);				
				match = pt_num.matcher(line);
				
				if(match.find()){
					row_num = Integer.parseInt(match.group());
				}
				
				if(match.find()){
					col_num = Integer.parseInt(match.group());
				}
				
				MPO = new String[row_num];
				matrix = new int[row_num][col_num];
				
				for(int i=0; i<row_num; i++){
					line = br.readLine();
					String[] value = line.split("\t");  //读入MPO的矩阵
					//System.out.println(value[0]);
					for(int j=0; j<col_num; j++){						 
						MPO[i] = value[0]; //value[0]为MPO注释
						if(value[j+1].equals("-")){//check
							matrix[i][j] = 2;
						}else{
							matrix[i][j] = Integer.parseInt(value[j+1]);
						}
//						System.out.println(j);
					}
				}
								
				count++;

				double[][] distance = new double[row_num][row_num];
				double dmax = 0.0;
				
				//wr.write(row_num + "\n");
				wr2.write("MPO_count\tSuper_MPO_count\tMPO\tSuper_MPO\n");
				
				for(int m=0; m<row_num; m++){
//				System.out.println("   Processing Row " + (m+1) + " of Matrix " + count);
					//wr.write(MPO[m]);
					for(int n=0; n<=m; n++){
						if(n < m){
							int na = 0;
							int nb = 0;
							int nab = 0;
							for(int x=0; x<col_num; x++){					
								if(matrix[m][x] == 1 && matrix[n][x]!= 2) na++;
								if(matrix[n][x] == 1 && matrix[m][x]!= 2) nb++;
								if(matrix[m][x] == 1 && matrix[n][x] == 1) nab++;
							}							
//							System.out.println("m=" + m + "\tn=" + n + "\t" + na + "\t" + nb + "\t" + nab);
							
							double na_double = na;
							double nb_double = nb;
							double nab_double = nab;
							
							if(nab == 0){
								distance[m][n] = maxDistance;
								// wr3.write(MPO[m] + "\t" + MPO[n] + "\n");
							}else{
								
								if(nab == na){
									//System.out.println(MPO[m] + " is part of " + MPO[n]);
									wr2.write(nab + "\t" + nb + "\t" + MPO[m] + "\t" + MPO[n] + "\n");
								}
							
								if(nab == nb){
									//System.out.println(MPO[n] + " is part of " + MPO[m]);
									wr2.write(nab + "\t" + na + "\t" + MPO[n] + "\t" + MPO[m] + "\n");
								}
								
								double doa = -(Math.log(nab_double/na_double));
								double dob = -(Math.log(nab_double/nb_double));
								double dab = Math.sqrt(doa*dob);
								
								distance[m][n] = dab;
								dmax = Math.max(dmax, dab);
//							System.out.println(dmax);
								BigDecimal bigdec = new BigDecimal(distance[m][n], new MathContext(7));
								wr.write(MPO[m] + "\t" + MPO[n] + "\t" + bigdec + "\n");
							}
							
						}else {
							distance[m][n] = 0;
							//wr.write(MPO[m] + "\t" + MPO[n] + "\t" + distance[m][n] + "\n");
						}
					}
					wr.flush();
					wr2.flush();
					//wr3.flush();
				}
			}
		}
		br.close();
		wr.close();
		wr2.close();
		//wr3.close();
	}
}
