package com.logicaldoc.util.security;

import org.junit.Test;

public class PasswordGeneratorTest {

	@Test
	public void testGenerate() {
		
		for(int i=0; i<3; i++) {
			PasswordGenerator pg = new PasswordGenerator();
			String xx = pg.generate(8);
			System.out.println(xx);
		}		
	}

}
