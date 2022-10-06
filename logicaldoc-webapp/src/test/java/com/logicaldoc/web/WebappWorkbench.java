package com.logicaldoc.web;

import java.io.IOException;
import java.util.regex.Pattern;


public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
//		String acceptHeader="                                                "
//				+ "                                                       "
//				+ "                                             "
//				+ "                                             "
//				+ "                                            "
//				+ "                                             "
//				+ "";
//		
//		String[] acceptValues = acceptHeader.split("\\s*(,|;)\\s*");
		

//		java.util.regex.Pattern.compile("\\s*(,|;)\\s*").matcher(
//				"                                                                                                       "+
//				"                                                                     "+
//				"\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n,\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n\\t\\n"+
//				"                                                                                                       ,"+
//				",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"+
//				";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;").matches();
		
		
		java.util.regex.Pattern.compile("(h|h|ih(((i|a|c|c|a|i|i|j|b|a|i|b|a|a|j))+h)ahbfhba|c|i)*").matcher(
				"hchcchicihcchciiicichhcichcihcchiihichiciiiihhcchi"+
				"cchhcihchcihiihciichhccciccichcichiihcchcihhicchcciicchcccihiiihhihihihi"+
				"chicihhcciccchihhhcchichchciihiicihciihcccciciccicciiiiiiiiicihhhiiiihchccch"+
				"chhhhiiihchihcccchhhiiiiiiiicicichicihcciciihichhhhchihciiihhiccccccciciihh"+
				"ichiccchhicchicihihccichicciihcichccihhiciccccccccichhhhihihhcchchihih"+
				"iihhihihihicichihiiiihhhhihhhchhichiicihhiiiiihchccccchichci").matches();
		
	}
}
