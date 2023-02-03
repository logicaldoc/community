package com.logicaldoc.webservice.doc.service;

import static com.logicaldoc.webservice.doc.util.MyClassUtils.isClassArrayOrCollection;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javax.jws.WebParam;
import javax.jws.WebResult;

import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.doc.model.JavaLanguageVariable;
import com.logicaldoc.webservice.doc.util.GenericsUtils;

/**
 * 
 * @author chenjianjx
 * 
 */
public class JavaLanguageVariableFactory {

	/**
	 * Creates a variable from a field which is @XmlElement annotated
	 * 
	 * @param field the field
	 * 
	 * @return the variable
	 * 
	 */
	public static JavaLanguageVariable createVariableFromField(Field field) {
		JavaLanguageVariable variable = new JavaLanguageVariable();

		WSDoc annotation = field.getAnnotation(WSDoc.class);
		if (annotation == null) {
			variable.setVariableName(getElementName(field));
			variable.setRequired(true);
		} else {
			variable.setVariableName(getVariableName(field, annotation));
			variable.setRequired(isVariableRequired(annotation));
			variable.setDescription(getVariableDescription(field, annotation));
		}

		if (isClassArrayOrCollection((Class<?>) field.getType()))
			variable.setType(field.getType().getComponentType());
		else
			variable.setType(GenericsUtils.getFieldGenericType(field));
		variable.setMultiOccurs(isClassArrayOrCollection((Class<?>) field.getType()));
		return variable;
	}

	private static String getElementName(Field field) {
		return field.getName();
	}

	private static String getVariableName(Field field, WSDoc annotation) {
		if (!annotation.documented())
			return "sskip";
		else
			return getElementName(field);
	}

	private static String getVariableDescription(Field field, WSDoc annotation) {
		return annotation.description();
	}

	/**
	 * Creates variables from parameters which are @WebParam annotated
	 * 
	 * @param method the method
	 * 
	 * @return list of variables
	 */
	public static List<JavaLanguageVariable> createVariablesFromMethodParamaters(Method method) {
		List<JavaLanguageVariable> variables = new ArrayList<>();
		Annotation[][] annotationMatrix = method.getParameterAnnotations();
		for (int paramIndex = 0; paramIndex < annotationMatrix.length; paramIndex++) {
			Annotation[] annotationsForSingleParam = annotationMatrix[paramIndex];
			WebParam xmlAnnotation = getXmlAnnotation(annotationsForSingleParam);

			JavaLanguageVariable variable = buildJavaVariableFromMethodParam(method, paramIndex, xmlAnnotation);
			variables.add(variable);
		}
		return variables;
	}

	/**
	 * Creates a variable from a method's return which is @WebResult annotated
	 * 
	 * @param method the method
	 * 
	 * @return the vaiable
	 */
	public static JavaLanguageVariable createVariableFromMethodReturn(Method method) {

		WebResult webResultAnnotation = method.getAnnotation(WebResult.class);
		JavaLanguageVariable variable = new JavaLanguageVariable();
		variable.setType(GenericsUtils.getMethodGenericReturnType(method));

		variable.setVariableName(webResultAnnotation != null ? webResultAnnotation.name() : "");
		variable.setRequired(true);
		Class<?> resultClass = method.getReturnType();
		variable.setMultiOccurs(isClassArrayOrCollection(resultClass));
		return variable;

	}

	private static JavaLanguageVariable buildJavaVariableFromMethodParam(Method method, int paramIndex,
			WebParam xmlAnnotation) {
		JavaLanguageVariable variable = new JavaLanguageVariable();
		variable.setType(GenericsUtils.getMethodGenericParameterTypes(method, paramIndex));
		if (xmlAnnotation != null)
			variable.setVariableName(xmlAnnotation.name());
		else
			variable.setVariableName(GenericsUtils.getMethodGenericParameterName(method, paramIndex));

		WSDoc docAnnotation = method.getParameters()[paramIndex].getAnnotation(WSDoc.class);
		if (docAnnotation != null) {
			variable.setDescription(docAnnotation.description());
		}

		variable.setRequired(isVariableRequired(docAnnotation));

		Class<?> paramClass = method.getParameterTypes()[paramIndex];
		variable.setMultiOccurs(isClassArrayOrCollection(paramClass));
		return variable;
	}

	private static WebParam getXmlAnnotation(Annotation[] annotationForSingleParam) {
		for (Annotation annotation : annotationForSingleParam) {
			if (annotation instanceof WebParam) {
				return (WebParam) annotation;
			}
		}
		return null;
	}

	static boolean isVariableRequired(WSDoc annotation) {
		if (annotation == null)
			return true;
		else
			return annotation.required();
	}
}