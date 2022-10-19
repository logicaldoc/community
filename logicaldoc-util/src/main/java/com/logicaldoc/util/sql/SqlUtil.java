package com.logicaldoc.util.sql;

/**
 * This class contains methods about SQL handling
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.5
 */
public class SqlUtil {

	/**
	 * Double the quotes in a string. 
	 * This means that each character ' will be replaced with '' (two quote characters)
	 * 
	 * @param input The incoming string
	 * @return A string with doubled quotes
	 */
	public static String doubleQuotes(String input){
		return input.replace("'", "''");
	}
	
	public void initialize(int[] vector, int index){
		vector[index]=0;
		if(index<vector.length)
			initialize(vector, index++);
	}
}
