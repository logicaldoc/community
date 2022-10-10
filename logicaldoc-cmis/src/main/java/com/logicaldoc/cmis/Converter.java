package com.logicaldoc.cmis;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.data.ExtensionsData;
import org.apache.chemistry.opencmis.commons.definitions.Choice;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.FolderTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PolicyTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyBooleanDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDateTimeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDecimalDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyHtmlDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyIdDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyIntegerDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyStringDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyUriDefinition;
import org.apache.chemistry.opencmis.commons.definitions.RelationshipTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import org.apache.chemistry.opencmis.commons.enums.DateTimeResolution;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractPropertyDefinition;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractTypeDefinition;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChoiceImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.CmisExtensionElementImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.DocumentTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.FolderTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PolicyTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyBooleanDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDateTimeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDecimalDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyHtmlDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIntegerDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyUriDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RelationshipTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceBoolean;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceDateTime;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceDecimal;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceHtml;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceId;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceInteger;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceString;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisChoiceUri;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyBoolean;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyBooleanDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyDateTime;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyDateTimeDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyDecimal;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyDecimalDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyHtml;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyHtmlDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyId;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyIdDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyInteger;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyIntegerDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyString;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyStringDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyUri;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisPropertyUriDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisTypeDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisTypeDocumentDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisTypeFolderDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisTypePolicyDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisTypeRelationshipDefinitionType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.EnumBaseObjectTypeIds;
import org.apache.chemistry.opencmis.commons.impl.jaxb.EnumCardinality;
import org.apache.chemistry.opencmis.commons.impl.jaxb.EnumContentStreamAllowed;
import org.apache.chemistry.opencmis.commons.impl.jaxb.EnumDateTimeResolution;
import org.apache.chemistry.opencmis.commons.impl.jaxb.EnumPropertyType;
import org.apache.chemistry.opencmis.commons.impl.jaxb.EnumUpdatability;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Copied from the same class in OpenCMIS 0.6.0. Contains converter methods.
 */
public final class Converter {

	private static final String DEFAULT_EXTENSION_NS = "http://chemistry.apache.org/opencmis/extension";

	/**
	 * Private constructor.
	 */
	private Converter() {
	}

	// -------------------------------------------------------------------------
	// --- Types ---
	// -------------------------------------------------------------------------

	/**
	 * Converts a type definition object
	 * 
	 * @param typeDefinition definition of the type
	 * 
	 * @return the type definition obect
	 */
	public static TypeDefinition convert(CmisTypeDefinitionType typeDefinition) {
		if (typeDefinition == null)
			return null;

		AbstractTypeDefinition result = null;

		if (typeDefinition instanceof CmisTypeFolderDefinitionType) {
			result = new FolderTypeDefinitionImpl();
		} else if (typeDefinition instanceof CmisTypeDocumentDefinitionType) {
			result = new DocumentTypeDefinitionImpl();

			((DocumentTypeDefinitionImpl) result).setContentStreamAllowed(convert(ContentStreamAllowed.class,
					((CmisTypeDocumentDefinitionType) typeDefinition).getContentStreamAllowed()));
			((DocumentTypeDefinitionImpl) result)
					.setIsVersionable(((CmisTypeDocumentDefinitionType) typeDefinition).isVersionable());
		} else if (typeDefinition instanceof CmisTypeRelationshipDefinitionType) {
			result = new RelationshipTypeDefinitionImpl();

			((RelationshipTypeDefinitionImpl) result).setAllowedSourceTypes(
					((CmisTypeRelationshipDefinitionType) typeDefinition).getAllowedSourceTypes());
			((RelationshipTypeDefinitionImpl) result).setAllowedTargetTypes(
					((CmisTypeRelationshipDefinitionType) typeDefinition).getAllowedTargetTypes());
		} else if (typeDefinition instanceof CmisTypePolicyDefinitionType) {
			result = new PolicyTypeDefinitionImpl();
		} else {
			throw new CmisRuntimeException("Type '" + typeDefinition.getId() + "' does not match a base type!");
		}

		result.setBaseTypeId(convert(BaseTypeId.class, typeDefinition.getBaseId()));
		result.setDescription(typeDefinition.getDescription());
		result.setDisplayName(typeDefinition.getDisplayName());
		result.setId(typeDefinition.getId());
		result.setIsControllableAcl(typeDefinition.isControllableACL());
		result.setIsControllablePolicy(typeDefinition.isControllablePolicy());
		result.setIsCreatable(typeDefinition.isCreatable());
		result.setIsFileable(typeDefinition.isFileable());
		result.setIsFulltextIndexed(typeDefinition.isFulltextIndexed());
		result.setIsIncludedInSupertypeQuery(typeDefinition.isIncludedInSupertypeQuery());
		result.setIsQueryable(typeDefinition.isQueryable());
		result.setLocalName(typeDefinition.getLocalName());
		result.setLocalNamespace(typeDefinition.getLocalNamespace());
		result.setParentTypeId(typeDefinition.getParentId());
		result.setQueryName(typeDefinition.getQueryName());

		for (CmisPropertyDefinitionType propertyDefinition : typeDefinition.getPropertyDefinition())
			result.addPropertyDefinition(convert(propertyDefinition));

		// handle extensions
		convertExtension(typeDefinition, result);

		return result;
	}

	/**
	 * Converts a property definition object
	 * 
	 * @param propertyDefinition CMIS definition object
	 * 
	 * @return definition object
	 */
	private static PropertyDefinition<?> convert(CmisPropertyDefinitionType propertyDefinition) {
		if (propertyDefinition == null)
			return null;

		AbstractPropertyDefinition<?> result = null;

		if (propertyDefinition instanceof CmisPropertyStringDefinitionType) {
			result = new PropertyStringDefinitionImpl();

			((PropertyStringDefinitionImpl) result).setChoices(
					convertChoiceStringList(((CmisPropertyStringDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyString prop = ((CmisPropertyStringDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyStringDefinitionImpl) result).setDefaultValue(prop.getValue());
			}

			// specific
			((PropertyStringDefinitionImpl) result)
					.setMaxLength(((CmisPropertyStringDefinitionType) propertyDefinition).getMaxLength());
		} else if (propertyDefinition instanceof CmisPropertyIdDefinitionType) {
			result = new PropertyIdDefinitionImpl();

			((PropertyIdDefinitionImpl) result)
					.setChoices(convertChoiceIdList(((CmisPropertyIdDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyId prop = ((CmisPropertyIdDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyIdDefinitionImpl) result).setDefaultValue(prop.getValue());
			}
		} else if (propertyDefinition instanceof CmisPropertyIntegerDefinitionType) {
			result = new PropertyIntegerDefinitionImpl();

			((PropertyIntegerDefinitionImpl) result).setChoices(
					convertChoiceIntegerList(((CmisPropertyIntegerDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyInteger prop = ((CmisPropertyIntegerDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyIntegerDefinitionImpl) result).setDefaultValue(prop.getValue());
			}

			// specific
			((PropertyIntegerDefinitionImpl) result)
					.setMinValue(((CmisPropertyIntegerDefinitionType) propertyDefinition).getMinValue());
			((PropertyIntegerDefinitionImpl) result)
					.setMaxValue(((CmisPropertyIntegerDefinitionType) propertyDefinition).getMaxValue());
		} else if (propertyDefinition instanceof CmisPropertyDecimalDefinitionType) {
			result = new PropertyDecimalDefinitionImpl();

			((PropertyDecimalDefinitionImpl) result).setChoices(
					convertChoiceDecimalList(((CmisPropertyDecimalDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyDecimal prop = ((CmisPropertyDecimalDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyDecimalDefinitionImpl) result).setDefaultValue(prop.getValue());
			}

			// specific
			((PropertyDecimalDefinitionImpl) result)
					.setMinValue(((CmisPropertyDecimalDefinitionType) propertyDefinition).getMinValue());
			((PropertyDecimalDefinitionImpl) result)
					.setMaxValue(((CmisPropertyDecimalDefinitionType) propertyDefinition).getMaxValue());

		} else if (propertyDefinition instanceof CmisPropertyBooleanDefinitionType) {
			result = new PropertyBooleanDefinitionImpl();

			((PropertyBooleanDefinitionImpl) result).setChoices(
					convertChoiceBooleanList(((CmisPropertyBooleanDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyBoolean prop = ((CmisPropertyBooleanDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyBooleanDefinitionImpl) result).setDefaultValue(prop.getValue());
			}
		} else if (propertyDefinition instanceof CmisPropertyDateTimeDefinitionType) {
			result = new PropertyDateTimeDefinitionImpl();

			((PropertyDateTimeDefinitionImpl) result).setChoices(
					convertChoiceDateTimeList(((CmisPropertyDateTimeDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyDateTime prop = ((CmisPropertyDateTimeDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyDateTimeDefinitionImpl) result).setDefaultValue(convertXMLCalendar(prop.getValue()));
			}

			// specific
			((PropertyDateTimeDefinitionImpl) result).setDateTimeResolution(convert(DateTimeResolution.class,
					((CmisPropertyDateTimeDefinitionType) propertyDefinition).getResolution()));
		} else if (propertyDefinition instanceof CmisPropertyHtmlDefinitionType) {
			result = new PropertyHtmlDefinitionImpl();

			((PropertyHtmlDefinitionImpl) result).setChoices(
					convertChoiceHtmlList(((CmisPropertyHtmlDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyHtml prop = ((CmisPropertyHtmlDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyHtmlDefinitionImpl) result).setDefaultValue(prop.getValue());
			}
		} else if (propertyDefinition instanceof CmisPropertyUriDefinitionType) {
			result = new PropertyUriDefinitionImpl();

			((PropertyUriDefinitionImpl) result)
					.setChoices(convertChoiceUriList(((CmisPropertyUriDefinitionType) propertyDefinition).getChoice()));

			CmisPropertyUri prop = ((CmisPropertyUriDefinitionType) propertyDefinition).getDefaultValue();
			if (prop != null) {
				((PropertyUriDefinitionImpl) result).setDefaultValue(prop.getValue());
			}
		} else
			return null;

		result.setCardinality(convert(Cardinality.class, propertyDefinition.getCardinality()));
		result.setDescription(propertyDefinition.getDescription());
		result.setDisplayName(propertyDefinition.getDisplayName());
		result.setId(propertyDefinition.getId());
		result.setIsInherited(propertyDefinition.isInherited());
		result.setIsOpenChoice(propertyDefinition.isOpenChoice());
		result.setIsQueryable(propertyDefinition.isQueryable());
		result.setIsOrderable(propertyDefinition.isOrderable());
		result.setIsRequired(propertyDefinition.isRequired());
		result.setLocalName(propertyDefinition.getLocalName());
		result.setLocalNamespace(propertyDefinition.getLocalNamespace());
		result.setPropertyType(convert(PropertyType.class, propertyDefinition.getPropertyType()));
		result.setQueryName(propertyDefinition.getQueryName());
		result.setUpdatability(convert(Updatability.class, propertyDefinition.getUpdatability()));

		// handle extensions
		convertExtension(propertyDefinition, result);

		return result;
	}

	/**
	 * Converts a type definition object
	 * 
	 * @param typeDefinition type definition object
	 * 
	 * @return CMIS type definition object
	 */
	public static CmisTypeDefinitionType convert(TypeDefinition typeDefinition) {
		if (typeDefinition == null)
			return null;

		CmisTypeDefinitionType result = null;

		if (typeDefinition instanceof DocumentTypeDefinition) {
			result = new CmisTypeDocumentDefinitionType();

			DocumentTypeDefinition docTypeDefintion = (DocumentTypeDefinition) typeDefinition;
			((CmisTypeDocumentDefinitionType) result)
					.setVersionable(convertBoolean(docTypeDefintion.isVersionable(), false));
			((CmisTypeDocumentDefinitionType) result).setContentStreamAllowed(
					convert(EnumContentStreamAllowed.class, docTypeDefintion.getContentStreamAllowed()));
		} else if (typeDefinition instanceof FolderTypeDefinition) {
			result = new CmisTypeFolderDefinitionType();
		} else if (typeDefinition instanceof RelationshipTypeDefinition) {
			result = new CmisTypeRelationshipDefinitionType();

			RelationshipTypeDefinition relationshipTypeDefinition = (RelationshipTypeDefinition) typeDefinition;

			if (relationshipTypeDefinition.getAllowedSourceTypeIds() != null) {
				for (String type : relationshipTypeDefinition.getAllowedSourceTypeIds()) {
					((CmisTypeRelationshipDefinitionType) result).getAllowedSourceTypes().add(type);
				}
			}

			if (relationshipTypeDefinition.getAllowedTargetTypeIds() != null) {
				for (String type : relationshipTypeDefinition.getAllowedTargetTypeIds()) {
					((CmisTypeRelationshipDefinitionType) result).getAllowedTargetTypes().add(type);
				}
			}
		} else if (typeDefinition instanceof PolicyTypeDefinition) {
			result = new CmisTypePolicyDefinitionType();
		} else
			return null;

		result.setBaseId(convert(EnumBaseObjectTypeIds.class, typeDefinition.getBaseTypeId()));
		result.setControllableACL(convertBoolean(typeDefinition.isControllableAcl(), false));
		result.setControllablePolicy(convertBoolean(typeDefinition.isControllablePolicy(), false));
		result.setCreatable(convertBoolean(typeDefinition.isCreatable(), false));
		result.setDescription(typeDefinition.getDescription());
		result.setDisplayName(typeDefinition.getDisplayName());
		result.setFileable(convertBoolean(typeDefinition.isFileable(), false));
		result.setFulltextIndexed(convertBoolean(typeDefinition.isFulltextIndexed(), false));
		result.setId(typeDefinition.getId());
		result.setIncludedInSupertypeQuery(convertBoolean(typeDefinition.isIncludedInSupertypeQuery(), false));
		result.setLocalName(typeDefinition.getLocalName());
		result.setLocalNamespace(typeDefinition.getLocalNamespace());
		result.setParentId(typeDefinition.getParentTypeId());
		result.setQueryable(convertBoolean(typeDefinition.isQueryable(), false));
		result.setQueryName(typeDefinition.getQueryName());

		if (typeDefinition.getPropertyDefinitions() != null) {
			for (PropertyDefinition<?> propDef : typeDefinition.getPropertyDefinitions().values()) {
				result.getPropertyDefinition().add(convert(propDef));
			}
		}

		// handle extensions
		convertExtension(typeDefinition, result);

		return result;
	}

	/**
	 * Converts a property definition object
	 * 
	 * @param propertyDefinition definition of property
	 * 
	 * @return CMS property definition
	 */
	private static CmisPropertyDefinitionType convert(PropertyDefinition<?> propertyDefinition) {
		if (propertyDefinition == null)
			return null;

		CmisPropertyDefinitionType result = null;

		if (propertyDefinition instanceof PropertyStringDefinition) {
			result = new CmisPropertyStringDefinitionType();

			PropertyStringDefinition source = (PropertyStringDefinition) propertyDefinition;
			CmisPropertyStringDefinitionType target = (CmisPropertyStringDefinitionType) result;

			convertChoiceStringList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyString defaultValue = new CmisPropertyString();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (String value : source.getDefaultValue()) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}

			// specific
			target.setMaxLength(source.getMaxLength());
		} else if (propertyDefinition instanceof PropertyIdDefinition) {
			result = new CmisPropertyIdDefinitionType();

			PropertyIdDefinition source = (PropertyIdDefinition) propertyDefinition;
			CmisPropertyIdDefinitionType target = (CmisPropertyIdDefinitionType) result;

			convertChoiceIdList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyId defaultValue = new CmisPropertyId();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (String value : source.getDefaultValue()) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}
		} else if (propertyDefinition instanceof PropertyIntegerDefinition) {
			result = new CmisPropertyIntegerDefinitionType();

			PropertyIntegerDefinition source = (PropertyIntegerDefinition) propertyDefinition;
			CmisPropertyIntegerDefinitionType target = (CmisPropertyIntegerDefinitionType) result;

			convertChoiceIntegerList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyInteger defaultValue = new CmisPropertyInteger();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (BigInteger value : source.getDefaultValue()) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}

			// specific
			target.setMinValue(source.getMinValue());
			target.setMaxValue(source.getMaxValue());
		} else if (propertyDefinition instanceof PropertyDecimalDefinition) {
			result = new CmisPropertyDecimalDefinitionType();

			PropertyDecimalDefinition source = (PropertyDecimalDefinition) propertyDefinition;
			CmisPropertyDecimalDefinitionType target = (CmisPropertyDecimalDefinitionType) result;

			convertChoiceDecimalList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyDecimal defaultValue = new CmisPropertyDecimal();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (BigDecimal value : source.getDefaultValue()) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}

			// specific
			target.setMinValue(source.getMinValue());
			target.setMaxValue(source.getMaxValue());
			if (source.getPrecision() != null) {
				target.setPrecision(source.getPrecision().value());
			}
		} else if (propertyDefinition instanceof PropertyBooleanDefinition) {
			result = new CmisPropertyBooleanDefinitionType();

			PropertyBooleanDefinition source = (PropertyBooleanDefinition) propertyDefinition;
			CmisPropertyBooleanDefinitionType target = (CmisPropertyBooleanDefinitionType) result;

			convertChoiceBooleanList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyBoolean defaultValue = new CmisPropertyBoolean();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (Boolean value : source.getDefaultValue()) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}
		} else if (propertyDefinition instanceof PropertyDateTimeDefinition) {
			result = new CmisPropertyDateTimeDefinitionType();

			PropertyDateTimeDefinition source = (PropertyDateTimeDefinition) propertyDefinition;
			CmisPropertyDateTimeDefinitionType target = (CmisPropertyDateTimeDefinitionType) result;

			convertChoiceDateTimeList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyDateTime defaultValue = new CmisPropertyDateTime();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (XMLGregorianCalendar value : convertCalendar(source.getDefaultValue())) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}

			// specific
			target.setResolution(convert(EnumDateTimeResolution.class, source.getDateTimeResolution()));
		} else if (propertyDefinition instanceof PropertyHtmlDefinition) {
			result = new CmisPropertyHtmlDefinitionType();

			PropertyHtmlDefinition source = (PropertyHtmlDefinition) propertyDefinition;
			CmisPropertyHtmlDefinitionType target = (CmisPropertyHtmlDefinitionType) result;

			convertChoiceHtmlList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyHtml defaultValue = new CmisPropertyHtml();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (String value : source.getDefaultValue()) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}
		} else if (propertyDefinition instanceof PropertyUriDefinition) {
			result = new CmisPropertyUriDefinitionType();

			PropertyUriDefinition source = (PropertyUriDefinition) propertyDefinition;
			CmisPropertyUriDefinitionType target = (CmisPropertyUriDefinitionType) result;

			convertChoiceUriList(source.getChoices(), target.getChoice());

			if (source.getDefaultValue() != null) {
				CmisPropertyUri defaultValue = new CmisPropertyUri();
				defaultValue.setPropertyDefinitionId(propertyDefinition.getId());
				for (String value : source.getDefaultValue()) {
					defaultValue.getValue().add(value);
				}
				target.setDefaultValue(defaultValue);
			}
		} else
			return null;

		result.setCardinality(convert(EnumCardinality.class, propertyDefinition.getCardinality()));
		result.setDescription(propertyDefinition.getDescription());
		result.setDisplayName(propertyDefinition.getDisplayName());
		result.setId(propertyDefinition.getId());
		result.setInherited(propertyDefinition.isInherited());
		result.setLocalName(propertyDefinition.getLocalName());
		result.setLocalNamespace(propertyDefinition.getLocalNamespace());
		result.setOpenChoice(propertyDefinition.isOpenChoice());
		result.setOrderable(convertBoolean(propertyDefinition.isOrderable(), false));
		result.setPropertyType(convert(EnumPropertyType.class, propertyDefinition.getPropertyType()));
		result.setQueryable(convertBoolean(propertyDefinition.isQueryable(), false));
		result.setQueryName(propertyDefinition.getQueryName());
		result.setRequired(convertBoolean(propertyDefinition.isRequired(), false));
		result.setUpdatability(convert(EnumUpdatability.class, propertyDefinition.getUpdatability()));

		// handle extensions
		convertExtension(propertyDefinition, result);

		return result;
	}

	// -------------------------------------------------------------------------
	// --- Choices ---
	// -------------------------------------------------------------------------

	/**
	 * Converts a choices list
	 * 
	 * @param choices the list of choices
	 * 
	 * @return list of choices
	 */
	private static List<Choice<String>> convertChoiceStringList(List<CmisChoiceString> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<String>> result = new ArrayList<>();

		for (CmisChoiceString choice : choices) {
			ChoiceImpl<String> newChoice = new ChoiceImpl<String>();

			newChoice.setChoice(convertChoiceStringList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(choice.getValue());

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * @param target the list that will receive the CMIS choices strings
	 */
	private static void convertChoiceStringList(List<Choice<String>> choices, List<CmisChoiceString> target) {
		if (choices == null) {
			return;
		}

		for (Choice<String> choice : choices) {
			CmisChoiceString newChoice = new CmisChoiceString();

			convertChoiceStringList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (String value : choice.getValue()) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * 
	 * @return list of choices
	 */
	private static List<Choice<String>> convertChoiceIdList(List<CmisChoiceId> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<String>> result = new ArrayList<>();

		for (CmisChoiceId choice : choices) {
			ChoiceImpl<String> newChoice = new ChoiceImpl<String>();

			newChoice.setChoice(convertChoiceIdList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(choice.getValue());

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * @param target listr that will receive the conversion
	 */
	private static void convertChoiceIdList(List<Choice<String>> choices, List<CmisChoiceId> target) {
		if (choices == null)
			return;

		for (Choice<String> choice : choices) {
			CmisChoiceId newChoice = new CmisChoiceId();

			convertChoiceIdList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (String value : choice.getValue()) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * 
	 * @return the list of conversions
	 */
	private static List<Choice<BigInteger>> convertChoiceIntegerList(List<CmisChoiceInteger> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<BigInteger>> result = new ArrayList<>();

		for (CmisChoiceInteger choice : choices) {
			ChoiceImpl<BigInteger> newChoice = new ChoiceImpl<>();

			newChoice.setChoice(convertChoiceIntegerList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(choice.getValue());

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * @param target list that will receive the conversions
	 */
	private static void convertChoiceIntegerList(List<Choice<BigInteger>> choices, List<CmisChoiceInteger> target) {
		if (choices == null)
			return;

		for (Choice<BigInteger> choice : choices) {
			CmisChoiceInteger newChoice = new CmisChoiceInteger();

			convertChoiceIntegerList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (BigInteger value : choice.getValue()) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * 
	 * @return the list of conversions
	 */
	private static List<Choice<BigDecimal>> convertChoiceDecimalList(List<CmisChoiceDecimal> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<BigDecimal>> result = new ArrayList<>();

		for (CmisChoiceDecimal choice : choices) {
			ChoiceImpl<BigDecimal> newChoice = new ChoiceImpl<>();

			newChoice.setChoice(convertChoiceDecimalList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(choice.getValue());

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the coices
	 * @param target list that will receive the conversions
	 */
	private static void convertChoiceDecimalList(List<Choice<BigDecimal>> choices, List<CmisChoiceDecimal> target) {
		if (choices == null)
			return;

		for (Choice<BigDecimal> choice : choices) {
			CmisChoiceDecimal newChoice = new CmisChoiceDecimal();

			convertChoiceDecimalList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (BigDecimal value : choice.getValue()) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the coices
	 * @return list of conversions
	 */
	private static List<Choice<Boolean>> convertChoiceBooleanList(List<CmisChoiceBoolean> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<Boolean>> result = new ArrayList<>();

		for (CmisChoiceBoolean choice : choices) {
			ChoiceImpl<Boolean> newChoice = new ChoiceImpl<Boolean>();

			newChoice.setChoice(convertChoiceBooleanList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(choice.getValue());

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * @param target list that will receive the conversions
	 */
	private static void convertChoiceBooleanList(List<Choice<Boolean>> choices, List<CmisChoiceBoolean> target) {
		if (choices == null)
			return;

		for (Choice<Boolean> choice : choices) {
			CmisChoiceBoolean newChoice = new CmisChoiceBoolean();

			convertChoiceBooleanList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (Boolean value : choice.getValue()) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	/**
	 * Converts a choices list
	 * 
	 * @param choices the choices
	 * 
	 * @return the list of conversions
	 */
	private static List<Choice<GregorianCalendar>> convertChoiceDateTimeList(List<CmisChoiceDateTime> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<GregorianCalendar>> result = new ArrayList<Choice<GregorianCalendar>>();

		for (CmisChoiceDateTime choice : choices) {
			ChoiceImpl<GregorianCalendar> newChoice = new ChoiceImpl<GregorianCalendar>();

			newChoice.setChoice(convertChoiceDateTimeList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(convertXMLCalendar(choice.getValue()));

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list.
	 */
	private static void convertChoiceDateTimeList(List<Choice<GregorianCalendar>> choices,
			List<CmisChoiceDateTime> target) {
		if (choices == null)
			return;

		for (Choice<GregorianCalendar> choice : choices) {
			CmisChoiceDateTime newChoice = new CmisChoiceDateTime();

			convertChoiceDateTimeList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (XMLGregorianCalendar value : convertCalendar(choice.getValue())) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	/**
	 * Converts a choices list.
	 */
	private static List<Choice<String>> convertChoiceHtmlList(List<CmisChoiceHtml> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<String>> result = new ArrayList<Choice<String>>();

		for (CmisChoiceHtml choice : choices) {
			ChoiceImpl<String> newChoice = new ChoiceImpl<String>();

			newChoice.setChoice(convertChoiceHtmlList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(choice.getValue());

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list.
	 */
	private static void convertChoiceHtmlList(List<Choice<String>> choices, List<CmisChoiceHtml> target) {
		if (choices == null)
			return;

		for (Choice<String> choice : choices) {
			CmisChoiceHtml newChoice = new CmisChoiceHtml();

			convertChoiceHtmlList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (String value : choice.getValue()) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	/**
	 * Converts a choices list.
	 */
	private static List<Choice<String>> convertChoiceUriList(List<CmisChoiceUri> choices) {
		if (choices == null)
			return new ArrayList<>();

		List<Choice<String>> result = new ArrayList<>();

		for (CmisChoiceUri choice : choices) {
			ChoiceImpl<String> newChoice = new ChoiceImpl<>();

			newChoice.setChoice(convertChoiceUriList(choice.getChoice()));
			newChoice.setDisplayName(choice.getDisplayName());
			newChoice.setValue(choice.getValue());

			result.add(newChoice);
		}

		return result;
	}

	/**
	 * Converts a choices list.
	 */
	private static void convertChoiceUriList(List<Choice<String>> choices, List<CmisChoiceUri> target) {
		if (choices == null)
			return;

		for (Choice<String> choice : choices) {
			CmisChoiceUri newChoice = new CmisChoiceUri();

			convertChoiceUriList(choice.getChoice(), newChoice.getChoice());
			newChoice.setDisplayName(choice.getDisplayName());

			if (choice.getValue() != null) {
				for (String value : choice.getValue()) {
					newChoice.getValue().add(value);
				}
			}

			target.add(newChoice);
		}
	}

	// -------------------------------------------------------------------------
	// --- Objects ---
	// -------------------------------------------------------------------------

	/**
	 * Converts a CMIS object
	 * 
	 * @param object the object to convert
	 * 
	 * @return the converted object
	 */
//	private static ObjectData convert(CmisObjectType object) {
//		if (object == null)
//			return null;
//
//		ObjectDataImpl result = new ObjectDataImpl();
//
//		result.setAcl(convert(object.getAcl(), object.isExactACL()));
//		result.setAllowableActions(convert(object.getAllowableActions()));
//		if (object.getChangeEventInfo() != null) {
//			ChangeEventInfoDataImpl changeEventInfo = new ChangeEventInfoDataImpl();
//			if (object.getChangeEventInfo().getChangeTime() != null) {
//				changeEventInfo.setChangeTime(object.getChangeEventInfo().getChangeTime().toGregorianCalendar());
//			}
//			changeEventInfo.setChangeType(convert(ChangeType.class, object.getChangeEventInfo().getChangeType()));
//			convertExtension(object.getChangeEventInfo(), changeEventInfo);
//
//			result.setChangeEventInfo(changeEventInfo);
//		}
//		result.setIsExactAcl(object.isExactACL());
//		result.setPolicyIds(convert(object.getPolicyIds()));
//		result.setProperties(convert(object.getProperties()));
//		List<ObjectData> relationships = new ArrayList<ObjectData>();
//		for (CmisObjectType cmisObject : object.getRelationship()) {
//			relationships.add(convert(cmisObject));
//		}
//		result.setRelationships(relationships);
//		List<RenditionData> renditions = new ArrayList<RenditionData>();
//		for (CmisRenditionType rendition : object.getRendition()) {
//			renditions.add(convert(rendition));
//		}
//		result.setRenditions(renditions);
//
//		// handle extensions
//		convertExtension(object, result);
//
//		return result;
//	}

	/**
	 * Converts a properties object
	 * 
	 * @param properties the object to convert
	 * 
	 * @return the converted properties object
	 */
//	private static Properties convert(CmisPropertiesType properties) {
//		if (properties == null)
//			return null;
//
//		PropertiesImpl result = new PropertiesImpl();
//
//		for (CmisProperty property : properties.getProperty()) {
//			result.addProperty(convert(property));
//		}
//
//		// handle extensions
//		convertExtension(properties, result);
//
//		return result;
//	}

	/**
	 * Converts a property object
	 * 
	 * @param property the property
	 * 
	 * @return the conversion
	 */
//	private static PropertyData<?> convert(CmisProperty property) {
//		if (property == null)
//			return null;
//
//		PropertyData<?> result = null;
//
//		if (property instanceof CmisPropertyString) {
//			result = new PropertyStringImpl(property.getPropertyDefinitionId(),
//					((CmisPropertyString) property).getValue());
//		} else if (property instanceof CmisPropertyId) {
//			result = new PropertyIdImpl(property.getPropertyDefinitionId(), ((CmisPropertyId) property).getValue());
//		} else if (property instanceof CmisPropertyInteger) {
//			result = new PropertyIntegerImpl(property.getPropertyDefinitionId(),
//					((CmisPropertyInteger) property).getValue());
//		} else if (property instanceof CmisPropertyDecimal) {
//			result = new PropertyDecimalImpl(property.getPropertyDefinitionId(),
//					((CmisPropertyDecimal) property).getValue());
//		} else if (property instanceof CmisPropertyBoolean) {
//			result = new PropertyBooleanImpl(property.getPropertyDefinitionId(),
//					((CmisPropertyBoolean) property).getValue());
//		} else if (property instanceof CmisPropertyDateTime) {
//			result = new PropertyDateTimeImpl(property.getPropertyDefinitionId(),
//					convertXMLCalendar(((CmisPropertyDateTime) property).getValue()));
//		} else if (property instanceof CmisPropertyHtml) {
//			result = new PropertyHtmlImpl(property.getPropertyDefinitionId(), ((CmisPropertyHtml) property).getValue());
//		} else if (property instanceof CmisPropertyUri) {
//			result = new PropertyUriImpl(property.getPropertyDefinitionId(), ((CmisPropertyUri) property).getValue());
//		} else {
//			return null;
//		}
//
//		((AbstractPropertyData<?>) result).setLocalName(property.getLocalName());
//		((AbstractPropertyData<?>) result).setQueryName(property.getQueryName());
//		((AbstractPropertyData<?>) result).setDisplayName(property.getDisplayName());
//
//		// handle extensions
//		convertExtension(property, result);
//
//		return result;
//	}

	/**
	 * Converts a properties object
	 * 
	 * @param properties the properties
	 * 
	 * @return the conversion
	 */
//	private static CmisPropertiesType convert(Properties properties) {
//		if (properties == null)
//			return null;
//
//		CmisPropertiesType result = new CmisPropertiesType();
//
//		if (properties.getProperties() != null) {
//			for (PropertyData<?> property : properties.getProperties().values()) {
//				result.getProperty().add(convert(property));
//			}
//		}
//
//		// handle extensions
//		convertExtension(properties, result);
//
//		return result;
//	}

	/**
	 * Converts a property object
	 * 
	 * @param property the property
	 * 
	 * @return the conversion
	 */
//	private static CmisProperty convert(PropertyData<?> property) {
//		if (property == null)
//			return null;
//
//		CmisProperty result = null;
//
//		if (property instanceof PropertyString) {
//			result = new CmisPropertyString();
//			((CmisPropertyString) result).getValue().addAll(((PropertyString) property).getValues());
//		} else if (property instanceof PropertyId) {
//			result = new CmisPropertyId();
//			((CmisPropertyId) result).getValue().addAll(((PropertyId) property).getValues());
//		} else if (property instanceof PropertyInteger) {
//			result = new CmisPropertyInteger();
//			((CmisPropertyInteger) result).getValue().addAll(((PropertyInteger) property).getValues());
//		} else if (property instanceof PropertyDecimal) {
//			result = new CmisPropertyDecimal();
//			((CmisPropertyDecimal) result).getValue().addAll(((PropertyDecimal) property).getValues());
//		} else if (property instanceof PropertyBoolean) {
//			result = new CmisPropertyBoolean();
//			((CmisPropertyBoolean) result).getValue().addAll(((PropertyBoolean) property).getValues());
//		} else if (property instanceof PropertyDateTime) {
//			result = new CmisPropertyDateTime();
//			((CmisPropertyDateTime) result).getValue()
//					.addAll(convertCalendar(((PropertyDateTime) property).getValues()));
//		} else if (property instanceof PropertyHtml) {
//			result = new CmisPropertyHtml();
//			((CmisPropertyHtml) result).getValue().addAll(((PropertyHtml) property).getValues());
//		} else if (property instanceof PropertyUri) {
//			result = new CmisPropertyUri();
//			((CmisPropertyUri) result).getValue().addAll(((PropertyUri) property).getValues());
//		} else
//			return null;
//
//		result.setPropertyDefinitionId(property.getId());
//		result.setLocalName(property.getLocalName());
//		result.setQueryName(property.getQueryName());
//		result.setDisplayName(property.getDisplayName());
//
//		return result;
//	}

	/**
	 * Converts a rendition object
	 * 
	 * @param rendition the rendition
	 * 
	 * @return the conversion
	 */
//	private static RenditionData convert(CmisRenditionType rendition) {
//		if (rendition == null)
//			return null;
//
//		RenditionDataImpl result = new RenditionDataImpl();
//
//		result.setBigHeight(rendition.getHeight());
//		result.setKind(rendition.getKind());
//		result.setBigLength(rendition.getLength());
//		result.setMimeType(rendition.getMimetype());
//		result.setRenditionDocumentId(rendition.getRenditionDocumentId());
//		result.setStreamId(rendition.getStreamId());
//		result.setTitle(rendition.getTitle());
//		result.setBigWidth(rendition.getWidth());
//
//		// handle extensions
//		convertExtension(rendition, result);
//
//		return result;
//	}

	/**
	 * Converts a rendition object
	 * 
	 * @param rendition the rendition
	 * 
	 * @return the conversion
	 */
//	private static CmisRenditionType convert(RenditionData rendition) {
//		if (rendition == null)
//			return null;
//
//		CmisRenditionType result = new CmisRenditionType();
//
//		result.setHeight(rendition.getBigHeight());
//		result.setKind(rendition.getKind());
//		result.setLength(rendition.getBigLength());
//		result.setMimetype(rendition.getMimeType());
//		result.setRenditionDocumentId(rendition.getRenditionDocumentId());
//		result.setStreamId(rendition.getStreamId());
//		result.setTitle(rendition.getTitle());
//		result.setWidth(rendition.getBigWidth());
//
//		// handle extensions
//		convertExtension(rendition, result);
//
//		return result;
//	}

	/**
	 * Converts a CMIS object
	 * 
	 * @param object the object to convert
	 * 
	 * @return the CMIS conversion
	 */
//	private static CmisObjectType convert(ObjectData object) {
//		if (object == null)
//			return null;
//
//		CmisObjectType result = new CmisObjectType();
//
//		result.setAcl(convert(object.getAcl()));
//		result.setAllowableActions(convert(object.getAllowableActions()));
//		if (object.getChangeEventInfo() != null) {
//			CmisChangeEventType changeEventInfo = new CmisChangeEventType();
//
//			changeEventInfo
//					.setChangeType(convert(EnumTypeOfChanges.class, object.getChangeEventInfo().getChangeType()));
//			changeEventInfo.setChangeTime(convertCalendar(object.getChangeEventInfo().getChangeTime()));
//
//			convertExtension(object.getChangeEventInfo(), changeEventInfo);
//
//			result.setChangeEventInfo(changeEventInfo);
//		}
//		result.setExactACL(object.getAcl() == null ? null : object.getAcl().isExact());
//		result.setPolicyIds(convert(object.getPolicyIds()));
//		result.setProperties(convert(object.getProperties()));
//		if (object.getRelationships() != null) {
//			for (ObjectData relationship : object.getRelationships()) {
//				result.getRelationship().add(convert(relationship));
//			}
//		}
//		if (object.getRenditions() != null) {
//			for (RenditionData rendition : object.getRenditions()) {
//				result.getRendition().add(convert(rendition));
//			}
//		}
//
//		// handle extensions
//		convertExtension(object, result);
//
//		return result;
//	}

	// -------------------------------------------------------------------------
	// --- ACLs and Policies ---
	// -------------------------------------------------------------------------

	/**
	 * Converts an ACL object with its ACEs
	 * 
	 * @param acl the security policies
	 * @param isExact if the specification is exact
	 * 
	 * @return the converted ACL
	 */
//	private static Acl convert(CmisAccessControlListType acl, Boolean isExact) {
//		if (acl == null)
//			return null;
//
//		AccessControlListImpl result = new AccessControlListImpl();
//
//		List<Ace> aces = new ArrayList<Ace>();
//		for (CmisAccessControlEntryType entry : acl.getPermission()) {
//			if (entry == null) {
//				continue;
//			}
//
//			AccessControlEntryImpl ace = new AccessControlEntryImpl();
//			ace.setDirect(entry.isDirect());
//			ace.setPermissions(entry.getPermission());
//			AccessControlPrincipalDataImpl principal = new AccessControlPrincipalDataImpl(
//					entry.getPrincipal() == null ? null : entry.getPrincipal().getPrincipalId());
//			convertExtension(entry.getPrincipal(), principal);
//			ace.setPrincipal(principal);
//
//			// handle extensions
//			convertExtension(entry, ace);
//
//			aces.add(ace);
//		}
//
//		result.setAces(aces);
//
//		result.setExact(isExact);
//
//		// handle extensions
//		convertExtension(acl, result);
//
//		return result;
//	}

	/**
	 * Converts an ACL object with its ACEs
	 * 
	 * @param acl the acl
	 * 
	 * @return the CMIS ACL
	 */
//	private static CmisAccessControlListType convert(Acl acl) {
//		if (acl == null) {
//			return null;
//		}
//
//		CmisAccessControlListType result = new CmisAccessControlListType();
//
//		if (acl.getAces() != null) {
//			for (Ace ace : acl.getAces()) {
//				if (ace == null) {
//					continue;
//				}
//
//				CmisAccessControlEntryType entry = new CmisAccessControlEntryType();
//
//				if (ace.getPrincipal() != null) {
//					CmisAccessControlPrincipalType pincipal = new CmisAccessControlPrincipalType();
//
//					pincipal.setPrincipalId(ace.getPrincipal().getId());
//					convertExtension(pincipal, ace.getPrincipal());
//
//					entry.setPrincipal(pincipal);
//				}
//
//				entry.setDirect(ace.isDirect());
//				entry.getPermission().addAll(ace.getPermissions());
//
//				convertExtension(ace, entry);
//
//				result.getPermission().add(entry);
//			}
//		}
//
//		// handle extensions
//		convertExtension(acl, result);
//
//		return result;
//	}

	/**
	 * Converts an AllowableActions object
	 * 
	 * @param allowableActions the allowable actions
	 * 
	 * @return the actions
	 */
//	private static AllowableActions convert(CmisAllowableActionsType allowableActions) {
//		if (allowableActions == null)
//			return null;
//
//		AllowableActionsImpl result = new AllowableActionsImpl();
//
//		Set<Action> set = EnumSet.noneOf(Action.class);
//
//		if (Boolean.TRUE.equals(allowableActions.isCanSetContentStream())) {
//			set.add(Action.CAN_SET_CONTENT_STREAM);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanAddObjectToFolder())) {
//			set.add(Action.CAN_ADD_OBJECT_TO_FOLDER);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanApplyACL())) {
//			set.add(Action.CAN_APPLY_ACL);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanApplyPolicy())) {
//			set.add(Action.CAN_APPLY_POLICY);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanCancelCheckOut())) {
//			set.add(Action.CAN_CANCEL_CHECK_OUT);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanCheckIn())) {
//			set.add(Action.CAN_CHECK_IN);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanCheckOut())) {
//			set.add(Action.CAN_CHECK_OUT);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanCreateDocument())) {
//			set.add(Action.CAN_CREATE_DOCUMENT);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanCreateFolder())) {
//			set.add(Action.CAN_CREATE_FOLDER);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanCreateRelationship())) {
//			set.add(Action.CAN_CREATE_RELATIONSHIP);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanDeleteContentStream())) {
//			set.add(Action.CAN_DELETE_CONTENT_STREAM);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanDeleteObject())) {
//			set.add(Action.CAN_DELETE_OBJECT);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanDeleteTree())) {
//			set.add(Action.CAN_DELETE_TREE);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetACL())) {
//			set.add(Action.CAN_GET_ACL);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetAllVersions())) {
//			set.add(Action.CAN_GET_ALL_VERSIONS);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetAppliedPolicies())) {
//			set.add(Action.CAN_GET_APPLIED_POLICIES);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetChildren())) {
//			set.add(Action.CAN_GET_CHILDREN);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetContentStream())) {
//			set.add(Action.CAN_GET_CONTENT_STREAM);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetDescendants())) {
//			set.add(Action.CAN_GET_DESCENDANTS);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetFolderParent())) {
//			set.add(Action.CAN_GET_FOLDER_PARENT);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetFolderTree())) {
//			set.add(Action.CAN_GET_FOLDER_TREE);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetObjectParents())) {
//			set.add(Action.CAN_GET_OBJECT_PARENTS);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetObjectRelationships())) {
//			set.add(Action.CAN_GET_OBJECT_RELATIONSHIPS);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetProperties())) {
//			set.add(Action.CAN_GET_PROPERTIES);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanGetRenditions())) {
//			set.add(Action.CAN_GET_RENDITIONS);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanMoveObject())) {
//			set.add(Action.CAN_MOVE_OBJECT);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanRemoveObjectFromFolder())) {
//			set.add(Action.CAN_REMOVE_OBJECT_FROM_FOLDER);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanRemovePolicy())) {
//			set.add(Action.CAN_REMOVE_POLICY);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanSetContentStream())) {
//			set.add(Action.CAN_SET_CONTENT_STREAM);
//		}
//		if (Boolean.TRUE.equals(allowableActions.isCanUpdateProperties())) {
//			set.add(Action.CAN_UPDATE_PROPERTIES);
//		}
//
//		result.setAllowableActions(set);
//
//		// handle extensions
//		convertExtension(allowableActions, result);
//
//		return result;
//	}

	/**
	 * Converts an AllowableActions object
	 * 
	 * @param allowableActions the object to convert
	 * 
	 * @return the conversion
	 */
//	private static CmisAllowableActionsType convert(AllowableActions allowableActions) {
//		if (allowableActions == null)
//			return null;
//
//		CmisAllowableActionsType result = new CmisAllowableActionsType();
//
//		if (allowableActions.getAllowableActions() != null) {
//			Set<Action> set = allowableActions.getAllowableActions();
//
//			result.setCanAddObjectToFolder(set.contains(Action.CAN_ADD_OBJECT_TO_FOLDER));
//			result.setCanApplyACL(set.contains(Action.CAN_APPLY_ACL));
//			result.setCanApplyPolicy(set.contains(Action.CAN_APPLY_POLICY));
//			result.setCanCancelCheckOut(set.contains(Action.CAN_CANCEL_CHECK_OUT));
//			result.setCanCheckIn(set.contains(Action.CAN_CHECK_IN));
//			result.setCanCheckOut(set.contains(Action.CAN_CHECK_OUT));
//			result.setCanCreateDocument(set.contains(Action.CAN_CREATE_DOCUMENT));
//			result.setCanCreateFolder(set.contains(Action.CAN_CREATE_FOLDER));
//			result.setCanCreateRelationship(set.contains(Action.CAN_CREATE_RELATIONSHIP));
//			result.setCanDeleteContentStream(set.contains(Action.CAN_DELETE_CONTENT_STREAM));
//			result.setCanDeleteObject(set.contains(Action.CAN_DELETE_OBJECT));
//			result.setCanDeleteTree(set.contains(Action.CAN_DELETE_TREE));
//			result.setCanGetACL(set.contains(Action.CAN_GET_ACL));
//			result.setCanGetAllVersions(set.contains(Action.CAN_GET_ALL_VERSIONS));
//			result.setCanGetAppliedPolicies(set.contains(Action.CAN_GET_APPLIED_POLICIES));
//			result.setCanGetChildren(set.contains(Action.CAN_GET_CHILDREN));
//			result.setCanGetContentStream(set.contains(Action.CAN_GET_CONTENT_STREAM));
//			result.setCanGetDescendants(set.contains(Action.CAN_GET_DESCENDANTS));
//			result.setCanGetFolderParent(set.contains(Action.CAN_GET_FOLDER_PARENT));
//			result.setCanGetFolderTree(set.contains(Action.CAN_GET_FOLDER_TREE));
//			result.setCanGetObjectParents(set.contains(Action.CAN_GET_OBJECT_PARENTS));
//			result.setCanGetObjectRelationships(set.contains(Action.CAN_GET_OBJECT_RELATIONSHIPS));
//			result.setCanGetProperties(set.contains(Action.CAN_GET_PROPERTIES));
//			result.setCanGetRenditions(set.contains(Action.CAN_GET_RENDITIONS));
//			result.setCanMoveObject(set.contains(Action.CAN_MOVE_OBJECT));
//			result.setCanRemoveObjectFromFolder(set.contains(Action.CAN_REMOVE_OBJECT_FROM_FOLDER));
//			result.setCanRemovePolicy(set.contains(Action.CAN_REMOVE_POLICY));
//			result.setCanSetContentStream(set.contains(Action.CAN_SET_CONTENT_STREAM));
//			result.setCanUpdateProperties(set.contains(Action.CAN_UPDATE_PROPERTIES));
//
//		}
//
//		// handle extensions
//		convertExtension(allowableActions, result);
//
//		return result;
//	}

	/**
	 * Converts a list of policy ids
	 * 
	 * @param policyIds identifiers of the policies
	 * 
	 * @return list of policy identifiers
	 */
//	private static PolicyIdList convert(CmisListOfIdsType policyIds) {
//		if (policyIds == null)
//			return null;
//
//		PolicyIdListImpl result = new PolicyIdListImpl();
//		result.setPolicyIds(policyIds.getId());
//
//		// handle extensions
//		convertExtension(policyIds, result);
//
//		return result;
//	}

	/**
	 * Converts a list of policy ids
	 * 
	 * @param policyIds list of policy identifiers
	 * 
	 * @return list of policy identifiers
	 */
//	private static CmisListOfIdsType convert(PolicyIdList policyIds) {
//		if (policyIds == null)
//			return null;
//
//		CmisListOfIdsType result = new CmisListOfIdsType();
//		if (policyIds.getPolicyIds() != null) {
//			for (String id : policyIds.getPolicyIds()) {
//				result.getId().add(id);
//			}
//		}
//
//		// handle extensions
//		convertExtension(policyIds, result);
//
//		return result;
//	}

//	/**
//	 * Converts a list of policy ids
//	 * 
//	 * @param policyIds list of policy identifiers
//	 * 
//	 * @return list of policy identifiers
//	 */
//	private static CmisListOfIdsType convertPolicyIds(List<String> policyIds) {
//		if (policyIds == null) {
//			return null;
//		}
//
//		CmisListOfIdsType result = new CmisListOfIdsType();
//		result.getId().addAll(policyIds);
//
//		return result;
//	}

	// -------------------------------------------------------------------------
	// --- Lists, containers and similar ---
	// -------------------------------------------------------------------------

	/**
	 * Converts a list of calendar objects
	 * 
	 * @param calendar list of gregorian XML calendars
	 * 
	 * @return list of gregorian calendars
	 */
	private static List<GregorianCalendar> convertXMLCalendar(List<XMLGregorianCalendar> calendar) {
		if (calendar == null)
			return new ArrayList<>();

		List<GregorianCalendar> result = new ArrayList<GregorianCalendar>();
		for (XMLGregorianCalendar cal : calendar) {
			if (cal != null) {
				result.add(cal.toGregorianCalendar());
			}
		}

		return result;
	}

	/**
	 * Converts a list of calendar objects
	 * 
	 * @param calendar the list of gregorian calendars
	 * 
	 * @return list of XML gregorian calendars
	 */
	private static List<XMLGregorianCalendar> convertCalendar(List<GregorianCalendar> calendar) {
		if (calendar == null)
			return new ArrayList<>();

		DatatypeFactory df;
		try {
			df = DatatypeFactory.newInstance();
		} catch (DatatypeConfigurationException e) {
			throw new CmisRuntimeException("Convert exception: " + e.getMessage(), e);
		}

		List<XMLGregorianCalendar> result = new ArrayList<XMLGregorianCalendar>();
		for (GregorianCalendar cal : calendar) {
			result.add(df.newXMLGregorianCalendar(cal));
		}

		return result;
	}

	/**
	 * Converts a calendar object
	 * 
	 * @param calendar gregorian calendar
	 * 
	 * @return an XML gregorian calendar
	 */
//	private static XMLGregorianCalendar convertCalendar(GregorianCalendar calendar) {
//		if (calendar == null)
//			return null;
//
//		DatatypeFactory df;
//		try {
//			df = DatatypeFactory.newInstance();
//		} catch (DatatypeConfigurationException e) {
//			throw new CmisRuntimeException("Convert exception: " + e.getMessage(), e);
//		}
//
//		return df.newXMLGregorianCalendar(calendar);
//	}

	/**
	 * Converts a type list
	 * 
	 * @param typeList the list to convert
	 * 
	 * @return the converted definition list
	 */
//	private static TypeDefinitionList convert(CmisTypeDefinitionListType typeList) {
//		if (typeList == null)
//			return null;
//
//		TypeDefinitionListImpl result = new TypeDefinitionListImpl();
//		List<TypeDefinition> types = new ArrayList<TypeDefinition>();
//		for (CmisTypeDefinitionType typeDefinition : typeList.getTypes()) {
//			types.add(convert(typeDefinition));
//		}
//
//		result.setList(types);
//		result.setHasMoreItems(typeList.isHasMoreItems());
//		result.setNumItems(typeList.getNumItems());
//
//		// handle extensions
//		convertExtension(typeList, result);
//
//		return result;
//	}

	/**
	 * Converts a type list
	 * 
	 * @param typeList list of types
	 * 
	 * @return the CMIS definitions
	 */
//	private static CmisTypeDefinitionListType convert(TypeDefinitionList typeList) {
//		if (typeList == null) {
//			return null;
//		}
//
//		CmisTypeDefinitionListType result = new CmisTypeDefinitionListType();
//
//		if (typeList.getList() != null) {
//			for (TypeDefinition tdd : typeList.getList()) {
//				result.getTypes().add(convert(tdd));
//			}
//		}
//
//		result.setHasMoreItems(convertBoolean(typeList.hasMoreItems(), false));
//		result.setNumItems(typeList.getNumItems());
//
//		// handle extensions
//		convertExtension(typeList, result);
//
//		return result;
//	}

	/**
	 * Converts a type container list
	 * 
	 * @param typeContainers the container that specifies the types
	 * 
	 * @return the list of type definitions
	 */
//	private static List<TypeDefinitionContainer> convertTypeContainerList(List<CmisTypeContainer> typeContainers) {
//		if (typeContainers == null)
//			return new ArrayList<>();
//
//		List<TypeDefinitionContainer> result = new ArrayList<TypeDefinitionContainer>();
//		for (CmisTypeContainer container : typeContainers) {
//			TypeDefinitionContainerImpl newConatiner = new TypeDefinitionContainerImpl();
//			newConatiner.setTypeDefinition(convert(container.getType()));
//			newConatiner.setChildren(convertTypeContainerList(container.getChildren()));
//			convertExtension(container, newConatiner);
//
//			result.add(newConatiner);
//		}
//
//		return result;
//	}

	/**
	 * Converts a type container list
	 * 
	 * @param typeContainers the container of types
	 * @param target the target list
	 */
//	private static void convertTypeContainerList(List<TypeDefinitionContainer> typeContainers,
//			List<CmisTypeContainer> target) {
//		if (typeContainers == null)
//			return;
//
//		for (TypeDefinitionContainer container : typeContainers) {
//			CmisTypeContainer newConatiner = new CmisTypeContainer();
//			newConatiner.setType(convert(container.getTypeDefinition()));
//			convertTypeContainerList(container.getChildren(), newConatiner.getChildren());
//			convertExtension(container, newConatiner);
//
//			target.add(newConatiner);
//		}
//	}

	/**
	 * Converts an ObjectInFolder object
	 * 
	 * @param objectInFolder CMIS object
	 * 
	 * @return the converted object
	 */
//	private static ObjectInFolderData convert(CmisObjectInFolderType objectInFolder) {
//		if (objectInFolder == null)
//			return null;
//
//		ObjectInFolderDataImpl result = new ObjectInFolderDataImpl();
//
//		result.setObject(convert(objectInFolder.getObject()));
//		result.setPathSegment(objectInFolder.getPathSegment());
//
//		// handle extensions
//		convertExtension(objectInFolder, result);
//
//		return result;
//	}

	/**
	 * Converts an ObjectInFolder object
	 * 
	 * @param objectInFolder the object to convert
	 * 
	 * @return the CMIS object
	 */
//	private static CmisObjectInFolderType convert(ObjectInFolderData objectInFolder) {
//		if (objectInFolder == null)
//			return null;
//
//		CmisObjectInFolderType result = new CmisObjectInFolderType();
//
//		result.setObject(convert(objectInFolder.getObject()));
//		result.setPathSegment(objectInFolder.getPathSegment());
//
//		// handle extensions
//		convertExtension(objectInFolder, result);
//
//		return result;
//	}

	/**
	 * Converts an ObjectInFolder container object
	 * 
	 * @param container the container
	 * 
	 * @return the CMIS object
	 */
//	private static ObjectInFolderContainer convert(CmisObjectInFolderContainerType container) {
//		if (container == null)
//			return null;
//
//		ObjectInFolderContainerImpl result = new ObjectInFolderContainerImpl();
//
//		result.setObject(convert(container.getObjectInFolder()));
//
//		List<ObjectInFolderContainer> containerList = new ArrayList<ObjectInFolderContainer>();
//		for (CmisObjectInFolderContainerType containerChild : container.getChildren()) {
//			containerList.add(convert(containerChild));
//		}
//
//		result.setChildren(containerList);
//
//		// handle extensions
//		convertExtension(container, result);
//
//		return result;
//	}

	/**
	 * Converts an ObjectInFolder container object
	 * 
	 * @param container the container
	 * 
	 * @return the CMIS object
	 */
//	private static CmisObjectInFolderContainerType convert(ObjectInFolderContainer container) {
//		if (container == null)
//			return null;
//
//		CmisObjectInFolderContainerType result = new CmisObjectInFolderContainerType();
//
//		result.setObjectInFolder(convert(container.getObject()));
//
//		if (container.getChildren() != null) {
//			for (ObjectInFolderContainer child : container.getChildren()) {
//				result.getChildren().add(convert(child));
//			}
//		}
//
//		// handle extensions
//		convertExtension(container, result);
//
//		return result;
//	}

	// -------------------------------------------------------------------------
	// --- Extensions and holders ---
	// -------------------------------------------------------------------------

	@SuppressWarnings("unchecked")
	private static void convertExtension(Object source, ExtensionsData target) {
		if (source == null)
			return;

		try {
			Method m = source.getClass().getMethod("getAny", new Class<?>[0]);
			List<Object> list = (List<Object>) m.invoke(source, new Object[0]);

			if (!list.isEmpty()) {
				List<CmisExtensionElement> extensions = new ArrayList<CmisExtensionElement>();
				for (Object obj : list) {
					extensions.add(convertDomToCmisExtensionElement(obj));
				}

				target.setExtensions(extensions);
			} else {
				target.setExtensions(null);
			}
		} catch (NoSuchMethodException e) {
		} catch (Exception e) {
			throw new CmisRuntimeException("Exception: " + e.getMessage(), e);
		}
	}

	@SuppressWarnings("unchecked")
	private static void convertExtension(ExtensionsData source, Object target) {
		if (source == null)
			return;

		try {
			Method m = target.getClass().getMethod("getAny", new Class<?>[0]);
			List<Object> list = (List<Object>) m.invoke(target, new Object[0]);

			list.clear();
			if (source.getExtensions() != null) {
				for (CmisExtensionElement ext : source.getExtensions()) {
					list.add(convertCmisExtensionElementToNode(ext));
				}
			}
		} catch (NoSuchMethodException e) {
		} catch (Exception e) {
			throw new CmisRuntimeException("Exception: " + e.getMessage(), e);
		}
	}

	/**
	 * Converts a DOM node to a CMIS extension element
	 * 
	 * @param source the source object
	 * 
	 * @return the CMIS element
	 */
	private static CmisExtensionElement convertDomToCmisExtensionElement(Object source) {
		// if it's not a Node, skip it
		if (!(source instanceof Node))
			return null;

		Node node = (Node) source;
		if (node.getNodeType() != Node.ELEMENT_NODE)
			return null;

		String name = node.getLocalName();
		String namespace = node.getNamespaceURI();

		CmisExtensionElement result = null;
		List<CmisExtensionElement> cmisChildren = new ArrayList<CmisExtensionElement>();
		StringBuilder value = new StringBuilder();

		NodeList children = node.getChildNodes();

		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);

			if (child.getNodeType() == Node.ELEMENT_NODE) {
				CmisExtensionElement cimsChild = convertDomToCmisExtensionElement(child);
				if (cimsChild != null) {
					cmisChildren.add(cimsChild);
				}
			} else if (child.getNodeType() == Node.TEXT_NODE) {
				value.append(child.getNodeValue());
			}
		}

		Map<String, String> attributes = null;
		if (node.getAttributes() != null) {
			attributes = new HashMap<String, String>();
			for (int i = 0; i < node.getAttributes().getLength(); i++) {
				Node attrNode = node.getAttributes().item(i);
				attributes.put(attrNode.getLocalName(), attrNode.getNodeValue());
			}
		}

		if (cmisChildren.isEmpty()) {
			result = new CmisExtensionElementImpl(namespace, name, attributes, value.toString());
		} else {
			result = new CmisExtensionElementImpl(namespace, name, attributes, cmisChildren);
		}

		return result;
	}

	/**
	 * Converts a CMIS extension element to a DOM node.
	 * 
	 * @param source extension element
	 * 
	 * @return the DOM node
	 */
	private static Node convertCmisExtensionElementToNode(CmisExtensionElement source) {
		if (source == null)
			return null;

		Document doc = null;

		try {
			DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
			doc = docBuilder.newDocument();
		} catch (Exception e) {
			throw new CmisRuntimeException("Unable to convert extensions!", e);
		}

		Element root = doc.createElementNS(
				(source.getNamespace() == null ? DEFAULT_EXTENSION_NS : source.getNamespace()), source.getName());
		doc.appendChild(root);

		if (source.getValue() != null) {
			root.appendChild(doc.createTextNode(source.getValue()));
		} else {
			for (CmisExtensionElement child : source.getChildren()) {
				root.appendChild(convertCmisExtensionElementToNode(child, root, doc));
			}
		}

		// set attributes
		if (source.getAttributes() != null) {
			for (Map.Entry<String, String> e : source.getAttributes().entrySet()) {
				root.setAttributeNS((source.getNamespace() == null ? DEFAULT_EXTENSION_NS : source.getNamespace()),
						e.getKey(), e.getValue());
			}
		}

		return root;
	}

	/**
	 * Converts a CMIS extension element to a DOM node
	 * 
	 * @param source the source extension
	 * @param parent the parent element
	 * @param doc the current document
	 * 
	 * @return the DOM node
	 */
	private static Node convertCmisExtensionElementToNode(CmisExtensionElement source, Element parent, Document doc) {
		if (source == null)
			return null;

		Element element = doc.createElementNS(
				(source.getNamespace() == null ? DEFAULT_EXTENSION_NS : source.getNamespace()), source.getName());

		if (source.getValue() != null) {
			element.appendChild(doc.createTextNode(source.getValue()));
		} else {
			for (CmisExtensionElement child : source.getChildren()) {
				element.appendChild(convertCmisExtensionElementToNode(child, element, doc));
			}
		}

		// set attributes
		if (source.getAttributes() != null) {
			for (Map.Entry<String, String> e : source.getAttributes().entrySet()) {
				element.setAttributeNS((source.getNamespace() == null ? DEFAULT_EXTENSION_NS : source.getNamespace()),
						e.getKey(), e.getValue());
			}
		}

		return element;
	}

	private static boolean convertBoolean(Boolean value, boolean def) {
		return (value == null ? def : value.booleanValue());
	}

	// -------------------------------------------------------------------------
	// --- Enums ---
	// -------------------------------------------------------------------------

	/**
	 * Converts an Enum
	 * 
	 * @param <T> the type
	 * @param destClass destination class
	 * @param source source enumeration
	 * 
	 * @return the converted enumeration
	 */
	private static <T extends Enum<T>> T convert(Class<T> destClass, Enum<?> source) {
		if (source == null) {
			return null;
		}
		return Enum.valueOf(destClass, source.name());
	}
}
