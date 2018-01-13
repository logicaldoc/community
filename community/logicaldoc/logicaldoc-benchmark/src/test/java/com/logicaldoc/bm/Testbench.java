package com.logicaldoc.bm;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import com.logicaldoc.util.StringUtil;


public class Testbench {

	public static void main(String[] args) throws IOException {
		List<String> strings=new ArrayList<String>();
		String buf = StringUtil.writeToString(Testbench.class.getResourceAsStream("/strings.txt"), "UTF-8");
		StringTokenizer st = new StringTokenizer(buf, "\n", false);
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			strings.add(token.trim());
		}
		
		for (String string : strings) {
			System.out.println(string);	
		}
		
	}

}
