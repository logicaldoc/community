package com.logicaldoc.webservice.doc.view.simple;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.webservice.doc.JavaNameDisplayStrategy;

public class SimpleJavaNameDisplayStrategy implements JavaNameDisplayStrategy {

	private static final Map<Class<?>, String> PRIMITIVE_TYPE_DISPLAY = new HashMap<>();
	static {
		PRIMITIVE_TYPE_DISPLAY.put(String.class, "String");

		PRIMITIVE_TYPE_DISPLAY.put(BigDecimal.class, "BigDecimal");
		PRIMITIVE_TYPE_DISPLAY.put(BigInteger.class, "BigInteger");
		PRIMITIVE_TYPE_DISPLAY.put(Byte.class, "Byte");
		PRIMITIVE_TYPE_DISPLAY.put(Double.class, "Double");
		PRIMITIVE_TYPE_DISPLAY.put(Float.class, "Float");
		PRIMITIVE_TYPE_DISPLAY.put(Integer.class, "Integer");
		PRIMITIVE_TYPE_DISPLAY.put(Long.class, "Long");
		PRIMITIVE_TYPE_DISPLAY.put(Short.class, "Short");

		PRIMITIVE_TYPE_DISPLAY.put(int.class, "Int");
		PRIMITIVE_TYPE_DISPLAY.put(long.class, "Long");
		PRIMITIVE_TYPE_DISPLAY.put(float.class, "Float");
		PRIMITIVE_TYPE_DISPLAY.put(double.class, "Double");
		PRIMITIVE_TYPE_DISPLAY.put(byte.class, "Byte");
		PRIMITIVE_TYPE_DISPLAY.put(char.class, "Char");
		PRIMITIVE_TYPE_DISPLAY.put(Boolean.class, "Boolean");
		PRIMITIVE_TYPE_DISPLAY.put(boolean.class, "Boolean");
		PRIMITIVE_TYPE_DISPLAY.put(Date.class, "Date");
		PRIMITIVE_TYPE_DISPLAY.put(Calendar.class, "Calendar");
		PRIMITIVE_TYPE_DISPLAY.put(XMLGregorianCalendar.class, "Calendar");
	}

	@Override
	public String displayElementName(String elementName) {
		if (!elementName.contains("-"))
			return elementName;
		else {
			String[] words = StringUtils.split(elementName, "-");

			List<String> capitalized = new ArrayList<>();
			for (String word : words) {
				capitalized.add(!capitalized.isEmpty() ? StringUtils.capitalize(word) : word);
			}

			return StringUtils.join(capitalized, "");
		}
	}

	@Override
	public String displayClassName(Class<?> clazz) {
		String className = clazz.getSimpleName();
		if (className.endsWith("Service"))
			className = className.substring(0, className.length() - "Service".length());
		System.out.println(className);

		return SimpleClassNameDisplayUtils.display(className);
	}

	@Override
	public String displayElementType(Class<?> type) {
		String display = PRIMITIVE_TYPE_DISPLAY.get(type);
		if (display != null) {
			return display;
		}
		if (type.isEnum()) {
			return StringUtils.join(Arrays.asList(type.getEnumConstants()), "<br/>");
		}

		String name = type.getName();
		if (name.contains("."))
			name = name.substring(name.lastIndexOf('.') + 1);
		return name;
	}
}