package com.logicaldoc.web;

import static com.cronutils.model.field.expression.FieldExpressionFactory.always;
import static com.cronutils.model.field.expression.FieldExpressionFactory.between;
import static com.cronutils.model.field.expression.FieldExpressionFactory.on;
import static com.cronutils.model.field.expression.FieldExpressionFactory.questionMark;

import java.io.IOException;
import java.util.Locale;

import com.cronutils.builder.CronBuilder;
import com.cronutils.descriptor.CronDescriptor;
import com.cronutils.model.Cron;
import com.cronutils.model.CronType;
import com.cronutils.model.definition.CronDefinition;
import com.cronutils.model.definition.CronDefinitionBuilder;
import com.cronutils.model.field.value.SpecialChar;
import com.cronutils.parser.CronParser;

public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
		String str="";
		for (int i = 1; i <= 31; i++) {
			str+="\""+i+"\",";
		};
		System.out.println(str);
		
		CronDefinition cronDefinition = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);
		Cron cron = CronBuilder.cron(cronDefinition).withYear(always())
				.withDoM(between(SpecialChar.L, 3)).withMonth(always()).withDoW(questionMark()).withHour(always())
				.withMinute(always()).withSecond(on(0)).instance();

		String cronAsString = cron.asString();

		CronDescriptor descriptor = CronDescriptor.instance(Locale.ITALIAN);

		CronParser parser = new CronParser(cronDefinition);

		// parse some expression and ask descriptor for description
		String description = descriptor.describe(parser.parse("0 0 12 1/1 pollo ? *"));

		System.out.println("Cron expression: " + cronAsString);
		System.out.println("Description: " + description);
	}
}
