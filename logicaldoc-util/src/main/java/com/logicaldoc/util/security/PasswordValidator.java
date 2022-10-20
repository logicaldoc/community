package com.logicaldoc.util.security;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.passay.CharacterOccurrencesRule;
import org.passay.CharacterRule;
import org.passay.EnglishCharacterData;
import org.passay.EnglishSequenceData;
import org.passay.IllegalSequenceRule;
import org.passay.LengthRule;
import org.passay.MessageResolver;
import org.passay.PasswordData;
import org.passay.PropertiesMessageResolver;
import org.passay.RuleResult;
import org.passay.WhitespaceRule;

/**
 * An utility class to validate the passwords
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class PasswordValidator {

	private org.passay.PasswordValidator validator;

	/**
	 * Constructor
	 * 
	 * @param length minimum size of the password
	 * @param uppercaseChars minimum number of upper case chars
	 * @param lowercaseChars minimum number of lower case chars
	 * @param digits minimum number of digits
	 * @param specialChars minimum number of special chars
	 * @param maxSequenceSize maximum size of a sequence
	 * @param maxOccurrences maximum number of occurrences of the same char
	 * @param messages optional map with error codes and messages
	 */
	public PasswordValidator(int length, int uppercaseChars, int lowercaseChars, int digits, int specialChars,
			int maxSequenceSize, int maxOccurrences, Properties messages) {

		MessageResolver resolver;
		if (messages == null) {
			Properties standardMessages = new Properties();
			try {
				standardMessages
						.load(this.getClass().getClassLoader().getResourceAsStream("/passay.properties"));
			} catch (Throwable e) {
				// Nothing to do
			}
			resolver = new PropertiesMessageResolver(standardMessages);
		} else
			resolver = new PropertiesMessageResolver(messages);

		validator = new org.passay.PasswordValidator(resolver,
				// length between X and 30 characters
				new LengthRule(length, 255),

				// at least X upper-case character
				new CharacterRule(EnglishCharacterData.UpperCase, uppercaseChars),

				// at least X lower-case characters
				new CharacterRule(EnglishCharacterData.LowerCase, lowercaseChars),

				// at least X digit characters
				new CharacterRule(EnglishCharacterData.Digit, digits),

				// at least X symbols (special character)
				new CharacterRule(EnglishCharacterData.Special, specialChars),

				// at least X times a character can be used
				new CharacterOccurrencesRule(maxOccurrences),

				// define some illegal sequences that will fail when >= 4 chars
				// long alphabetical is of the form 'abcd', numerical is '3456',
				// qwer
				// is 'asdf' the false parameter indicates that wrapped
				// sequences are
				// allowed; e.g. 'xyzab'
				new IllegalSequenceRule(EnglishSequenceData.Alphabetical, maxSequenceSize, false),
				new IllegalSequenceRule(EnglishSequenceData.Numerical, maxSequenceSize, false),
				new IllegalSequenceRule(EnglishSequenceData.USQwerty, maxSequenceSize, false),

				// no whitespace
				new WhitespaceRule());
	}

	/**
	 * Validates a password
	 * 
	 * @param password the password to check
	 * 
	 * @return the list of error messages or empty in case of valid password
	 */
	public List<String> validate(String password) {
		RuleResult result = validator.validate(new PasswordData(password));
		if (!result.isValid())
			return validator.getMessages(result);
		else
			return new ArrayList<String>();
	}
}
