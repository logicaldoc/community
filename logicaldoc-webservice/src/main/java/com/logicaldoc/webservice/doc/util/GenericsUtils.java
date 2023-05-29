package com.logicaldoc.webservice.doc.util;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * Get a variable's java type, or a collection's generic type<br>
 * For Example, <br>
 * "String a" =&gt; String.class, and "List&lt;Integer&gt; b" =&gt;
 * Integer.class
 * 
 */
public class GenericsUtils {

	private GenericsUtils() {
	}

	public static Class<?> getFieldGenericType(Field field) {
		return chooseOneType(field.getType(), field.getGenericType());
	}

	public static Class<?> getMethodGenericReturnType(Method method) {
		return chooseOneType(method.getReturnType(), method.getGenericReturnType());
	}

	public static Class<?> getMethodGenericParameterTypes(Method method, int paramIndex) {
		Class<?> literalType = method.getParameterTypes()[paramIndex];
		Type genericType = method.getGenericParameterTypes()[paramIndex];
		return chooseOneType(literalType, genericType);
	}

	public static String getMethodGenericParameterName(Method method, int paramIndex) {
		return method.getParameters()[paramIndex].getName();
	}

	static Class<?> chooseOneType(Class<?> literalType, Type genericType) {
		if (genericType instanceof ParameterizedType) {
			return digFromGenericType(genericType);
		}
		return literalType;
	}

	static Class<?> digFromGenericType(Type genericType) {
		Type[] typeArguments = ((ParameterizedType) genericType).getActualTypeArguments();
		Type type = typeArguments[0];
		if (type instanceof ParameterizedType) {
			return digFromGenericType(type);
		}

		if (type instanceof Class) {
			return (Class<?>) type;
		}

		return Object.class;

	}
}
