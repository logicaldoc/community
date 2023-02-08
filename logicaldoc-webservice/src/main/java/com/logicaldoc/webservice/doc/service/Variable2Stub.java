package com.logicaldoc.webservice.doc.service;

import static com.logicaldoc.webservice.doc.service.JavaLanguageVariableFactory.createVariableFromField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.webservice.doc.model.JavaLanguageVariable;
import com.logicaldoc.webservice.doc.model.Stub;
import com.logicaldoc.webservice.doc.model.StubTypeTree;

/**
 * Convert java language variables to stubs
 */
public class Variable2Stub {

	public static Stub convertToStub(JavaLanguageVariable variable, StubTypeTreeRepository typeTreeRepository) {
		Stub stub = new Stub();
		stub.setStubName(variable.getVariableName());
		stub.setRequired(variable.isRequired());
		stub.setMultiOccurs(variable.isMultiOccurs());
		stub.setDescription(variable.getDescription());
		stub.setType(variable.getType());

		Class<?> type = variable.getType();
		if (variable.getType().isArray())
			type = variable.getType().getComponentType();

		if (type.isAnnotationPresent(XmlType.class) && !type.isEnum()) {
			convertFieldsToChildStubs(stub, type, typeTreeRepository);
		}

		return stub;
	}

	private static void convertFieldsToChildStubs(Stub parentStub, Class<?> type,
			StubTypeTreeRepository typeTreeRepository) {
		for (Field childField : type.getDeclaredFields()) {
			Stub child = convertToStub(createVariableFromField(childField), typeTreeRepository);
			parentStub.addChild(child);
		}

		LinkedList<FieldsOfSubType> fieldsOfSubTypes = getFieldsOfSubTypes(type, typeTreeRepository);
		for (FieldsOfSubType fieldsOfSubType : fieldsOfSubTypes) {
			for (Field field : fieldsOfSubType.fields) {
				Stub childStub = convertToStub(createVariableFromField(field), typeTreeRepository);
				childStub.setSubTypeOfParentStub(fieldsOfSubType.subType);
				parentStub.addChild(childStub);
			}
		}

	}

	private static LinkedList<FieldsOfSubType> getFieldsOfSubTypes(Class<?> thisType,
			StubTypeTreeRepository typeTreeRepository) {
		LinkedList<FieldsOfSubType> fieldsOfAllSubTypes = new LinkedList<>();

		if (!thisType.isAnnotationPresent(XmlSeeAlso.class)) {
			return fieldsOfAllSubTypes;
		}

		List<Class<?>> subTypes = getSubTypes(thisType);

		registerToStubTypeTree(thisType, typeTreeRepository, subTypes);

		for (Class<?> subType : subTypes) {
			FieldsOfSubType fieldsOfSubType = new FieldsOfSubType(subType);
			fieldsOfSubType.addAll(Arrays.asList(subType.getDeclaredFields()));
			LinkedList<FieldsOfSubType> fieldsOfGrandSonType = getFieldsOfSubTypes(subType, typeTreeRepository);
			fieldsOfAllSubTypes.add(fieldsOfSubType);
			fieldsOfAllSubTypes.addAll(fieldsOfGrandSonType);
		}

		return fieldsOfAllSubTypes;
	}

	private static void registerToStubTypeTree(Class<?> thisType, StubTypeTreeRepository typeTreeRepository,
			List<Class<?>> subTypes) {
		StubTypeTree thisTypeTree = typeTreeRepository.getStubTypeTree(thisType);
		for (Class<?> subType : subTypes) {
			StubTypeTree subTypeTree = typeTreeRepository.getStubTypeTree(subType);
			subTypeTree.setParent(thisTypeTree);
		}
	}

	private static List<Class<?>> getSubTypes(Class<?> thisType) {
		List<Class<?>> subTypes = new ArrayList<>();
		for (Class<?> subType : thisType.getAnnotation(XmlSeeAlso.class).value()) {
			if (thisType.isAssignableFrom(subType)) {
				subTypes.add(subType);
			}
		}
		return subTypes;
	}

	private static final class FieldsOfSubType {
		private Class<?> subType;

		private List<Field> fields = new ArrayList<>();

		private FieldsOfSubType(Class<?> subType) {
			super();
			this.subType = subType;
		}

		public void addAll(List<Field> fields) {
			this.fields.addAll(fields);

		}

	}

}
