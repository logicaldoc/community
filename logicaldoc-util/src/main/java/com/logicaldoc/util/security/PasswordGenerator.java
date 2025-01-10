package com.logicaldoc.util.security;

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A password generator utility
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class PasswordGenerator {

	private static final String CHAR_LOWER = "abcdefghijklmnopqrstuvwxyz";

	private static final String CHAR_UPPER = CHAR_LOWER.toUpperCase();

	private static final String NUMBER = "0123456789";

	private static final String OTHER_CHAR = "!@#$%&*()_+-=[]?{}/;,.";

	private static final String PASSWORD_ALLOW_BASE = CHAR_LOWER + CHAR_UPPER + NUMBER + OTHER_CHAR;

	// optional, make it more random
	private static final String PASSWORD_ALLOW_BASE_SHUFFLE = shuffleString(PASSWORD_ALLOW_BASE);

	private static final String PASSWORD_ALLOW = PASSWORD_ALLOW_BASE_SHUFFLE;

	private static SecureRandom random = new SecureRandom();

	private PasswordGenerator() {
		throw new IllegalStateException("Utility class");
	}

	public static void main(String[] args) {
		Logger console = LoggerFactory.getLogger("console");

		console.info("String for password \t\t\t: {}", PASSWORD_ALLOW_BASE);
		console.info("String for password (shuffle) \t: {}", PASSWORD_ALLOW);

		// generate 5 random password
		for (int i = 0; i < 5; i++) {
			if (console.isInfoEnabled())
				console.info("password: {} \n", generate(16, 2, 2, 1, 1, 4, 2));
		}

	}

	private static String generate(int length) {
		if (length < 1)
			throw new IllegalArgumentException();

		StringBuilder sb = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			int rndCharAt = random.nextInt(PASSWORD_ALLOW.length());
			char rndChar = PASSWORD_ALLOW.charAt(rndCharAt);
			sb.append(rndChar);
		}

		return sb.toString();
	}

	// shuffle
	public static String shuffleString(String string) {
		List<String> letters = Arrays.asList(string.split(""));
		Collections.shuffle(letters);
		return letters.stream().collect(Collectors.joining());
	}

	/**
	 * Generates a new password
	 * 
	 * @param minLength minimum dimension of the password
	 * @param uppercaseChars minimum number of upper case chars
	 * @param lowercaseChars minimum number of lower case chars
	 * @param digits minimum number of digits
	 * @param specialChars minimum number of special chars
	 * @param maxSequenceSize maximum size of a sequence
	 * @param maxOccurrences maximum number of occurrences of the same char
	 * 
	 * @return the generated password
	 */
	public static String generate(int minLength, int uppercaseChars, int lowercaseChars, int digits, int specialChars,
			int maxSequenceSize, int maxOccurrences) {
		if (minLength < 5)
			throw new IllegalArgumentException(
					String.format("Cannot generate password with less than %d chars", minLength));

		PasswordCriteria criteria = new PasswordCriteria(minLength, uppercaseChars, lowercaseChars, digits,
				specialChars);
		criteria.setMaxSequenceSize(maxSequenceSize);
		criteria.setMaxOccurrences(maxOccurrences);

		PasswordValidator validator = new PasswordValidator(criteria, null);

		String pswd = "";
		boolean valid = false;
		int i = 0;
		while (!valid) {
			pswd = generate(minLength);
			valid = validator.validate(pswd).isEmpty();
			i++;
			if (i > 15) {
				i = 0;
				minLength++;
			}
		}
		return pswd;
	}
}
