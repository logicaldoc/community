package com.logicaldoc.web;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import org.passay.CharacterOccurrencesRule;
import org.passay.CharacterRule;
import org.passay.EnglishCharacterData;
import org.passay.EnglishSequenceData;
import org.passay.IllegalSequenceRule;
import org.passay.LengthRule;
import org.passay.MessageResolver;
import org.passay.PasswordData;
import org.passay.PasswordGenerator;
import org.passay.PasswordValidator;
import org.passay.PropertiesMessageResolver;
import org.passay.RuleResult;
import org.passay.WhitespaceRule;

import com.logicaldoc.i18n.I18N;

public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
//		String str = "";
//		for (int i = 1; i <= 31; i++) {
//			str += "\"" + i + "\",";
//		}
//		System.out.println(str);
//
//		CronDefinition cronDefinition = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);
//		Cron cron = CronBuilder.cron(cronDefinition).withYear(always()).withDoM(between(SpecialChar.L, 3))
//				.withMonth(always()).withDoW(questionMark()).withHour(always()).withMinute(always()).withSecond(on(0))
//				.instance();
//
//		String cronAsString = cron.asString();
//
//		CronDescriptor descriptor = CronDescriptor.instance(Locale.ITALIAN);
//
//		CronParser parser = new CronParser(cronDefinition);
//
//		// parse some expression and ask descriptor for description
//		String description = descriptor.describe(parser.parse("0 0 12 1/1 pollo ? *"));
//
//		System.out.println("Cron expression: " + cronAsString);
//		System.out.println("Description: " + description);

		passwordStuff();
	}

	private static void passwordStuff() {
		// String password = "RQTGH.;pa4ssword.L";
		String password = "lillocollo";

		List<CharacterRule> characterRules = new ArrayList<CharacterRule>();
		// at least X upper-case character
		characterRules.add(new CharacterRule(EnglishCharacterData.UpperCase, 2));

		// at least X lower-case characters
		characterRules.add(new CharacterRule(EnglishCharacterData.LowerCase, 2));

		// at least X digit characters
		characterRules.add(new CharacterRule(EnglishCharacterData.Digit, 1));

		// at least X symbols (special character)
		characterRules.add(new CharacterRule(EnglishCharacterData.Special, 1));

		Map<String, String> messages = I18N.getMessages(Locale.ITALIAN);
		List<String> errorKeys = messages.keySet().stream().filter(key -> key.startsWith("passwderror."))
				.collect(Collectors.toList());
		Properties props = new Properties();
		for (String key : errorKeys)
			props.put(key.substring(key.indexOf('.') + 1), messages.get(key));
		MessageResolver resolver = new PropertiesMessageResolver(props);
		PasswordValidator validator = new PasswordValidator(resolver,
				// length between X and 30 characters
				new LengthRule(8, 30),

				characterRules.get(0), characterRules.get(1), characterRules.get(2), characterRules.get(3),

				// define some illegal sequences that will fail when >= 4 chars
				// long
				// alphabetical is of the form 'abcd', numerical is '3456', qwer
				// is 'asdf'
				// the false parameter indicates that wrapped sequences are
				// allowed; e.g. 'xyzab'
				new IllegalSequenceRule(EnglishSequenceData.Alphabetical, 4, false),
				new IllegalSequenceRule(EnglishSequenceData.Numerical, 4, false),
				new IllegalSequenceRule(EnglishSequenceData.USQwerty, 4, false),

				new CharacterOccurrencesRule(1),

				// no whitespace
				new WhitespaceRule());

		RuleResult result = validator.validate(new PasswordData(password));
		System.out.println(validator.getMessages(result));

		PasswordGenerator generator = new PasswordGenerator();
		System.out.println(generator.generatePassword(12, characterRules));
	}
}
