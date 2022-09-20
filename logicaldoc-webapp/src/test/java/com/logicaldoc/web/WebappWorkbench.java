package com.logicaldoc.web;

import java.io.IOException;
import java.util.Locale;
import java.util.regex.Pattern;

import static com.cronutils.model.field.expression.FieldExpressionFactory.always;
import static com.cronutils.model.field.expression.FieldExpressionFactory.between;
import static com.cronutils.model.field.expression.FieldExpressionFactory.on;
import static com.cronutils.model.field.expression.FieldExpressionFactory.questionMark;

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
		
		String value="px2e%22-alert(1).\"-%22hnmlz'";
		Pattern scriptPattern = Pattern.compile("[\\)\"'\\.,;*?!%/\\(\\\\]", Pattern.CASE_INSENSITIVE);
		value = scriptPattern.matcher(value).replaceAll("");
System.out.println(value);
	}
}
