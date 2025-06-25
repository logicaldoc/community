package com.logicaldoc.util.security;

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

import com.logicaldoc.util.io.ResourceUtil;

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
	 * @param criteria minimum size of the password
	 * @param messages optional map with error codes and messages
	 */
	public PasswordValidator(PasswordCriteria criteria, Properties messages) {
		MessageResolver resolver;
		if (messages == null) {
			Properties standardMessages = new Properties();
			try {
				standardMessages.load(ResourceUtil.getInputStream("passay.properties"));
			} catch (Exception e) {
				// Nothing to do
			}
			resolver = new PropertiesMessageResolver(standardMessages);
		} else
			resolver = new PropertiesMessageResolver(messages);

		validator = new org.passay.PasswordValidator(resolver,
				// length between X and 30 characters
				new LengthRule(criteria.getMinLength(), 255),

				// at least X upper-case character
				new CharacterRule(EnglishCharacterData.UpperCase, criteria.getMinUppercaseChars()),

				// at least X lower-case characters
				new CharacterRule(EnglishCharacterData.LowerCase, criteria.getMinLowercaseChars()),

				// at least X digit characters
				new CharacterRule(EnglishCharacterData.Digit, criteria.getMinDigits()),

				// at least X symbols (special character)
				new CharacterRule(EnglishCharacterData.Special, criteria.getMinSpecialChars()),

				// at least X times a character can be used
				new CharacterOccurrencesRule(criteria.getMaxOccurrences()),

				// define some illegal sequences that will fail when >= 4 chars
				// long alphabetical is of the form 'abcd', numerical is '3456',
				// qwer
				// is 'asdf' the false parameter indicates that wrapped
				// sequences are
				// allowed; e.g. 'xyzab'
				new IllegalSequenceRule(EnglishSequenceData.Alphabetical, criteria.getMaxSequenceSize(), false),
				new IllegalSequenceRule(EnglishSequenceData.Numerical, criteria.getMaxSequenceSize(), false),
				new IllegalSequenceRule(EnglishSequenceData.USQwerty, criteria.getMaxSequenceSize(), false),

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
			return new ArrayList<>();
	}
}
