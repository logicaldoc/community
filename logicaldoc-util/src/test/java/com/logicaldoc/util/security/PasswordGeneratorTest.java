package com.logicaldoc.util.security;

import org.junit.Assert;
import org.junit.Test;

import junit.framework.TestCase;

/**
 * Test case for password generation
 * 
 * @author Marco Meschieri
 */
public class PasswordGeneratorTest extends TestCase {

	@Test
	public void testGenerate() {
		// Iterate the password generation for 10,000 times to see id every time
		// it is generated
		for (int i = 0; i < 10000; i++) {
			// Setup a generator with min 6 chars and restrictive conditions
			String password = PasswordGenerator.generate(5, 2, 2, 2, 2, 3, 1);
			Assert.assertNotNull(password);
			Assert.assertTrue(password.length() >= 5);
		}

		// Exception case: minLength < 5 should throw IllegalArgumentException
		try {
			PasswordGenerator.generate(4, 1, 1, 1, 1, 2, 1);
			Assert.fail("Expected IllegalArgumentException for minLength < 5");
		} catch (IllegalArgumentException e) {
			// ✅ expected
		}
	}
}
