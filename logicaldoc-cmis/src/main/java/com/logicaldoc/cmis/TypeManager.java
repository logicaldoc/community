package com.logicaldoc.cmis;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionContainer;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionList;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.WSConverter;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractPropertyDefinition;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractTypeDefinition;
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
import org.apache.chemistry.opencmis.commons.impl.dataobjects.TypeDefinitionContainerImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.TypeDefinitionListImpl;
import org.apache.chemistry.opencmis.commons.server.CallContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.util.Context;

/**
 * Type Manager.
 */
public class TypeManager {
	public static final String DOCUMENT_TYPE_ID = BaseTypeId.CMIS_DOCUMENT.value();

	public static final String FOLDER_TYPE_ID = BaseTypeId.CMIS_FOLDER.value();

	public static final String RELATIONSHIP_TYPE_ID = BaseTypeId.CMIS_RELATIONSHIP.value();

	public static final String POLICY_TYPE_ID = BaseTypeId.CMIS_POLICY.value();

	public static final String WORKSPACE_TYPE_ID = "Workspace";

	public static final String NAMESPACE = "http://logicaldoc.com/cmis";

	private static final Logger log = LoggerFactory.getLogger(TypeManager.class);

	private Map<String, TypeDefinitionContainerImpl> types;

	private List<TypeDefinitionContainer> typesList;

	public static final String PROP_LANGUAGE = "ldoc:language";

	public static final String PROP_RATING = "ldoc:rating";

	public static final String PROP_WORKFLOW_STATUS = "ldoc:workflowStatus";

	public static final String PROP_CUSTOMID = "ldoc:customId";

	public static final String PROP_TAGS = "ldoc:tags";

	public static final String PROP_DESCRIPTION = "ldoc:description";

	public static final String PROP_FILEVERSION = "ldoc:fileVersion";

	public static final String PROP_VERSION = "ldoc:version";

	public static final String PROP_TYPE = "ldoc:type";

	public static final String PROP_TEMPLATE = "ldoc:template";

	public static final String PROP_EXT = "ldoc:ext_";

	public TypeManager() {
		setup();
	}

	/**
	 * Creates the base types.
	 */
	private void setup() {
		types = new HashMap<>();
		typesList = new ArrayList<>();

		// folder type
		FolderTypeDefinitionImpl folderType = new FolderTypeDefinitionImpl();
		folderType.setBaseTypeId(BaseTypeId.CMIS_FOLDER);
		folderType.setIsControllableAcl(false);
		folderType.setIsControllablePolicy(false);
		folderType.setIsCreatable(true);
		final String folder = "Folder";
		folderType.setDescription(folder);
		folderType.setDisplayName(folder);
		folderType.setIsFileable(true);
		folderType.setIsFulltextIndexed(false);
		folderType.setIsIncludedInSupertypeQuery(true);
		folderType.setLocalName(folder);
		folderType.setLocalNamespace(NAMESPACE);
		folderType.setIsQueryable(true);
		folderType.setQueryName("cmis:folder");
		folderType.setId(FOLDER_TYPE_ID);

		addBasePropertyDefinitions(folderType);
		addFolderPropertyDefinitions(folderType);

		addTypeInteral(folderType);

		// Workspace type
		FolderTypeDefinitionImpl workspaceType = new FolderTypeDefinitionImpl();
		workspaceType.setBaseTypeId(BaseTypeId.CMIS_FOLDER);
		workspaceType.setIsControllableAcl(false);
		workspaceType.setIsControllablePolicy(false);
		workspaceType.setIsCreatable(false);
		workspaceType.setDescription(WORKSPACE_TYPE_ID);
		workspaceType.setDisplayName(WORKSPACE_TYPE_ID);
		workspaceType.setIsFileable(true);
		workspaceType.setIsFulltextIndexed(false);
		workspaceType.setIsIncludedInSupertypeQuery(true);
		workspaceType.setLocalName(WORKSPACE_TYPE_ID);
		workspaceType.setLocalNamespace(NAMESPACE);
		workspaceType.setIsQueryable(true);
		workspaceType.setQueryName(WORKSPACE_TYPE_ID);
		workspaceType.setId(WORKSPACE_TYPE_ID);

		addBasePropertyDefinitions(workspaceType);
		addFolderPropertyDefinitions(workspaceType);

		addTypeInteral(workspaceType);

		// document type
		DocumentTypeDefinitionImpl documentType = new DocumentTypeDefinitionImpl();
		documentType.setBaseTypeId(BaseTypeId.CMIS_DOCUMENT);
		documentType.setIsControllableAcl(false);
		documentType.setIsControllablePolicy(false);
		documentType.setIsCreatable(true);
		final String document = "Document";
		documentType.setDescription(document);
		documentType.setDisplayName(document);
		documentType.setIsFileable(true);
		documentType.setIsFulltextIndexed(false);
		documentType.setIsIncludedInSupertypeQuery(true);
		documentType.setLocalName(document);
		documentType.setLocalNamespace(NAMESPACE);
		documentType.setIsQueryable(true);
		documentType.setQueryName("cmis:document");
		documentType.setId(DOCUMENT_TYPE_ID);
		documentType.setIsVersionable(true);
		documentType.setContentStreamAllowed(ContentStreamAllowed.ALLOWED);

		addBasePropertyDefinitions(documentType);
		try {
			addDocumentPropertyDefinitions(documentType);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		addTypeInteral(documentType);

		// relationship types - not really supported
		RelationshipTypeDefinitionImpl relationshipType = new RelationshipTypeDefinitionImpl();
		relationshipType.setBaseTypeId(BaseTypeId.CMIS_RELATIONSHIP);
		relationshipType.setIsControllableAcl(false);
		relationshipType.setIsControllablePolicy(false);
		relationshipType.setIsCreatable(false);
		final String relationship = "Relationship";
		relationshipType.setDescription(relationship);
		relationshipType.setDisplayName(relationship);
		relationshipType.setIsFileable(false);
		relationshipType.setIsIncludedInSupertypeQuery(true);
		relationshipType.setLocalName(relationship);
		relationshipType.setLocalNamespace(NAMESPACE);
		relationshipType.setIsQueryable(false);
		relationshipType.setQueryName("cmis:relationship");
		relationshipType.setId(RELATIONSHIP_TYPE_ID);

		addBasePropertyDefinitions(relationshipType);
		addTypeInteral(relationshipType);

		// policy type
		PolicyTypeDefinitionImpl policyType = new PolicyTypeDefinitionImpl();
		policyType.setBaseTypeId(BaseTypeId.CMIS_POLICY);
		policyType.setIsControllableAcl(false);
		policyType.setIsControllablePolicy(false);
		policyType.setIsCreatable(false);
		final String policy = "Policy";
		policyType.setDescription(policy);
		policyType.setDisplayName(policy);
		policyType.setIsFileable(false);
		policyType.setIsIncludedInSupertypeQuery(true);
		policyType.setLocalName(policy);
		policyType.setLocalNamespace(NAMESPACE);
		policyType.setIsQueryable(false);
		policyType.setQueryName("cmis:policy");
		policyType.setId(POLICY_TYPE_ID);

		addBasePropertyDefinitions(policyType);
	}

	private static void addBasePropertyDefinitions(AbstractTypeDefinition type) {
		type.addPropertyDefinition(createPropDef(PropertyIds.BASE_TYPE_ID, "Base Type Id", "Base Type Id",
				PropertyType.ID, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.OBJECT_ID, "Object Id", "Object Id", PropertyType.ID,
				Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.OBJECT_TYPE_ID, "Type Id", "Type Id", PropertyType.ID,
				Cardinality.SINGLE, Updatability.ONCREATE, false, true));

		type.addPropertyDefinition(createPropDef(PropertyIds.NAME, "Name", "Name", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READWRITE, false, true));

		type.addPropertyDefinition(createPropDef(PropertyIds.CREATED_BY, "Created By", "Created By",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.CREATION_DATE, "Creation Date", "Creation Date",
				PropertyType.DATETIME, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.LAST_MODIFIED_BY, "Last Modified By", "Last Modified By",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(
				createPropDef(PropertyIds.LAST_MODIFICATION_DATE, "Last Modification Date", "Last Modification Date",
						PropertyType.DATETIME, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.CHANGE_TOKEN, "Change Token", "Change Token",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false));
	}

	private static void addFolderPropertyDefinitions(FolderTypeDefinitionImpl type) {
		type.addPropertyDefinition(createPropDef(PropertyIds.PARENT_ID, "Parent Id", "Parent Id", PropertyType.ID,
				Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.ALLOWED_CHILD_OBJECT_TYPE_IDS,
				"Allowed Child Object Type Ids", "Allowed Child Object Type Ids", PropertyType.ID, Cardinality.MULTI,
				Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.PATH, "Path", "Path", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READONLY, false, false));

		/*
		 * Properties of the LogicalDOC folder
		 */
		type.addPropertyDefinition(createPropDef(PROP_DESCRIPTION, "Description", "Description", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READWRITE, false, false));
		type.addPropertyDefinition(createPropDef(PROP_TYPE, "Type", "Type", PropertyType.INTEGER, Cardinality.SINGLE,
				Updatability.READONLY, false, false));
	}

	private static void addDocumentPropertyDefinitions(DocumentTypeDefinitionImpl type) throws PersistenceException {
		type.addPropertyDefinition(createPropDef(PropertyIds.IS_IMMUTABLE, "Is Immutable", "Is Immutable",
				PropertyType.BOOLEAN, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.IS_LATEST_VERSION, "Is Latest Version",
				"Is Latest Version", PropertyType.BOOLEAN, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.IS_MAJOR_VERSION, "Is Major Version", "Is Major Version",
				PropertyType.BOOLEAN, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(
				createPropDef(PropertyIds.IS_LATEST_MAJOR_VERSION, "Is Latest Major Version", "Is Latest Major Version",
						PropertyType.BOOLEAN, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.VERSION_LABEL, "Version Label", "Version Label",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, true));

		type.addPropertyDefinition(createPropDef(PropertyIds.VERSION_SERIES_ID, "Version Series Id",
				"Version Series Id", PropertyType.ID, Cardinality.SINGLE, Updatability.READONLY, false, true));

		type.addPropertyDefinition(createPropDef(PropertyIds.IS_VERSION_SERIES_CHECKED_OUT,
				"Is Verison Series Checked Out", "Is Verison Series Checked Out", PropertyType.BOOLEAN,
				Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.VERSION_SERIES_CHECKED_OUT_ID,
				"Version Series Checked Out Id", "Version Series Checked Out Id", PropertyType.ID, Cardinality.SINGLE,
				Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.VERSION_SERIES_CHECKED_OUT_BY,
				"Version Series Checked Out By", "Version Series Checked Out By", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.CHECKIN_COMMENT, "Checkin Comment", "Checkin Comment",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(
				createPropDef(PropertyIds.CONTENT_STREAM_LENGTH, "Content Stream Length", "Content Stream Length",
						PropertyType.INTEGER, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.CONTENT_STREAM_MIME_TYPE, "MIME Type", "MIME Type",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.CONTENT_STREAM_FILE_NAME, "Filename", "Filename",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false));

		type.addPropertyDefinition(createPropDef(PropertyIds.CONTENT_STREAM_ID, "Content Stream Id",
				"Content Stream Id", PropertyType.ID, Cardinality.SINGLE, Updatability.READONLY, false, false));

		/*
		 * Properties of all LogicalDOC documents
		 */
		type.addPropertyDefinition(createPropDef(PROP_FILEVERSION, "File Version", "File Version", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READONLY, false, false));
		type.addPropertyDefinition(createPropDef(PROP_VERSION, "Version", "Version", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READONLY, false, false));
		type.addPropertyDefinition(createPropDef(PROP_LANGUAGE, "Language", "Language", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READWRITE, false, false));
		type.addPropertyDefinition(createPropDef(PROP_WORKFLOW_STATUS, "Workflow Status", "Workflow Status",
				PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false));
		type.addPropertyDefinition(createPropDef(PROP_CUSTOMID, "Custom ID", "Custom Identifier", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READWRITE, false, false));
		type.addPropertyDefinition(createPropDef(PROP_RATING, "Rating", "Rating", PropertyType.INTEGER,
				Cardinality.SINGLE, Updatability.READONLY, false, false));
		type.addPropertyDefinition(createPropDef(PROP_TAGS, "Tags", "Tags", PropertyType.STRING, Cardinality.SINGLE,
				Updatability.READWRITE, false, false));
		type.addPropertyDefinition(createPropDef(PROP_TEMPLATE, "Template", "Template", PropertyType.STRING,
				Cardinality.SINGLE, Updatability.READWRITE, false, false));

		/*
		 * Extended properties
		 */
		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		List<AttributeSet> sets = dao.findAll();
		for (AttributeSet set : sets) {
			dao.initialize(set);
			Map<String, Attribute> attributes = set.getAttributes();
			for (String name : attributes.keySet()) {
				type.addPropertyDefinition(createPropDef(PROP_EXT + name, name, name, PropertyType.STRING,
						Cardinality.SINGLE, Updatability.READWRITE, false, false));
			}

		}
	}

	/**
	 * Creates a property definition object
	 * 
	 * @param id identifier of the property
	 * 
	 * @param displayName name to display
	 * @param description property's description
	 * @param datatype type or data
	 * @param cardinality the cardinality
	 * @param updateability if the property can be updated
	 * @param inherited if the property has been inherited
	 * @param required if the property is mandatory
	 * 
	 * @return the property definition
	 */
	private static PropertyDefinition<?> createPropDef(String id, String displayName, String description,
			PropertyType datatype, Cardinality cardinality, Updatability updateability, boolean inherited,
			boolean required) {
		AbstractPropertyDefinition<?> result = null;

		switch (datatype) {
		case BOOLEAN:
			result = new PropertyBooleanDefinitionImpl();
			break;
		case DATETIME:
			result = new PropertyDateTimeDefinitionImpl();
			break;
		case DECIMAL:
			result = new PropertyDecimalDefinitionImpl();
			break;
		case HTML:
			result = new PropertyHtmlDefinitionImpl();
			break;
		case ID:
			result = new PropertyIdDefinitionImpl();
			break;
		case INTEGER:
			result = new PropertyIntegerDefinitionImpl();
			break;
		case STRING:
			result = new PropertyStringDefinitionImpl();
			break;
		case URI:
			result = new PropertyUriDefinitionImpl();
			break;
		default:
			throw new IllegalArgumentException("Unknown datatype! Spec change?");
		}

		result.setId(id);
		result.setLocalName(id);
		result.setDisplayName(displayName);
		result.setDescription(description);
		result.setPropertyType(datatype);
		result.setCardinality(cardinality);
		result.setUpdatability(updateability);
		result.setIsInherited(inherited);
		result.setIsRequired(required);
		result.setIsQueryable(false);
		result.setIsOrderable(false);
		result.setQueryName(id);

		return result;
	}

	/**
	 * Adds a type to collection with inheriting base type properties
	 * 
	 * @param type the type
	 * 
	 * @return if the type has been added
	 */
	public boolean addType(TypeDefinition type) {
		if (type == null)
			return false;

		if (type.getBaseTypeId() == null)
			return false;

		// find base type
		TypeDefinition baseType = null;
		if (type.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {
			baseType = copyTypeDefintion(types.get(DOCUMENT_TYPE_ID).getTypeDefinition());
		} else if (type.getBaseTypeId() == BaseTypeId.CMIS_FOLDER) {
			baseType = copyTypeDefintion(types.get(FOLDER_TYPE_ID).getTypeDefinition());
		} else if (type.getBaseTypeId() == BaseTypeId.CMIS_RELATIONSHIP) {
			baseType = copyTypeDefintion(types.get(RELATIONSHIP_TYPE_ID).getTypeDefinition());
		} else if (type.getBaseTypeId() == BaseTypeId.CMIS_POLICY) {
			baseType = copyTypeDefintion(types.get(POLICY_TYPE_ID).getTypeDefinition());
		} else {
			return false;
		}

		AbstractTypeDefinition newType = (AbstractTypeDefinition) copyTypeDefintion(type);

		// copy property definition
		for (PropertyDefinition<?> propDef : baseType.getPropertyDefinitions().values()) {
			((AbstractPropertyDefinition<?>) propDef).setIsInherited(true);
			newType.addPropertyDefinition(propDef);
		}

		// add it
		addTypeInteral(newType);

		log.info("Added type '{}'", newType.getId());

		return true;
	}

	/**
	 * Adds a type to collection.
	 */
	private void addTypeInteral(AbstractTypeDefinition type) {
		if (type == null) {
			return;
		}

		if (types.containsKey(type.getId())) {
			// can't overwrite a type
			return;
		}

		TypeDefinitionContainerImpl tc = new TypeDefinitionContainerImpl();
		tc.setTypeDefinition(type);

		// add to parent
		if (type.getParentTypeId() != null) {
			TypeDefinitionContainerImpl tdc = types.get(type.getParentTypeId());
			if (tdc != null) {
				if (tdc.getChildren() == null) {
					tdc.setChildren(new ArrayList<>());
				}
				tdc.getChildren().add(tc);
			}
		}

		types.put(type.getId(), tc);
		typesList.add(tc);
	}

	/**
	 * CMIS getTypesChildren
	 * 
	 * @param context the call context
	 * @param typeId identifier of the type
	 * @param includePropertyDefinitions if the property definition must be
	 *        included
	 * @param maxItems maximum number of items
	 * @param skipCount optional number of potential results that the repository
	 *        MUST skip/page over before returning any results (default is 0)
	 * 
	 * @return list of definitions
	 */
	public TypeDefinitionList getTypesChildren(CallContext context, String typeId, boolean includePropertyDefinitions,
			BigInteger maxItems, BigInteger skipCount) {
		TypeDefinitionContainer tc = types.get(typeId);
		TypeDefinitionListImpl result = new TypeDefinitionListImpl(new ArrayList<>());
		if (typeId == null) {
			result.getList().add(copyTypeDefintion(types.get(FOLDER_TYPE_ID).getTypeDefinition()));
			result.getList().add(copyTypeDefintion(types.get(DOCUMENT_TYPE_ID).getTypeDefinition()));
		} else if (tc != null) {
			List<TypeDefinitionContainer> typeContainers = tc.getChildren().stream()
					.skip(skipCount != null ? skipCount.intValue() : 0)
					.limit(maxItems == null ? Integer.MAX_VALUE : maxItems.intValue()).toList();
			result = new TypeDefinitionListImpl(
					typeContainers.stream().map(c -> copyTypeDefintion(c.getTypeDefinition())).toList());
		}

		result.setHasMoreItems(tc != null && result.getList().size() < tc.getChildren().size());
		result.setNumItems(BigInteger.valueOf(tc != null ? tc.getChildren().size() : result.getList().size()));

		if (!includePropertyDefinitions) {
			for (TypeDefinition type : result.getList()) {
				type.getPropertyDefinitions().clear();
			}
		}

		return result;
	}

	/**
	 * CMIS getTypesDescendants
	 * 
	 * @param context call context
	 * @param typeId id of the type
	 * @param depth depth specification
	 * @param includePropertyDefinitions if the properties definition must be
	 *        included
	 * 
	 * @return list of definitions
	 */
	public List<TypeDefinitionContainer> getTypesDescendants(CallContext context, String typeId, BigInteger depth,
			Boolean includePropertyDefinitions) {
		List<TypeDefinitionContainer> result = new ArrayList<>();

		// check depth
		int d = (depth == null ? -1 : depth.intValue());
		if (d == 0) {
			throw new CmisInvalidArgumentException("Depth must not be 0!");
		}

		// set property definition flag to default value if not set
		boolean ipd = (includePropertyDefinitions != null && includePropertyDefinitions.booleanValue());

		if (typeId == null) {
			result.add(getTypesDescendants(d, types.get(FOLDER_TYPE_ID), ipd));
			result.add(getTypesDescendants(d, types.get(DOCUMENT_TYPE_ID), ipd));
		} else {
			TypeDefinitionContainer tc = types.get(typeId);
			if (tc != null) {
				result.add(getTypesDescendants(d, tc, ipd));
			}
		}

		return result;
	}

	/**
	 * Gathers the type descendants tree
	 * 
	 * @param depth depth specifications
	 * @param tc the container
	 * @param includePropertyDefinitions if the property definitions must be
	 *        included
	 * 
	 * @return the definition container
	 */
	private TypeDefinitionContainer getTypesDescendants(int depth, TypeDefinitionContainer tc,
			boolean includePropertyDefinitions) {
		TypeDefinitionContainerImpl result = new TypeDefinitionContainerImpl();

		TypeDefinition type = copyTypeDefintion(tc.getTypeDefinition());
		if (!includePropertyDefinitions) {
			type.getPropertyDefinitions().clear();
		}

		result.setTypeDefinition(type);

		if (depth != 0 && tc.getChildren() != null) {
			result.setChildren(new ArrayList<>());
			for (TypeDefinitionContainer tdc : tc.getChildren()) {
				result.getChildren()
						.add(getTypesDescendants(depth < 0 ? -1 : depth - 1, tdc, includePropertyDefinitions));
			}
		}

		return result;
	}

	/**
	 * For internal use
	 * 
	 * @param typeId identifier of the type
	 * 
	 * @return the type definition
	 */
	public TypeDefinition getType(String typeId) {
		TypeDefinitionContainer tc = types.get(typeId);
		if (tc == null) {
			return null;
		}

		return tc.getTypeDefinition();
	}

	/**
	 * CMIS getTypeDefinition
	 * 
	 * @param context call context
	 * @param typeId identifier of the type
	 * 
	 * @return definition of the type
	 */
	public TypeDefinition getTypeDefinition(CallContext context, String typeId) {
		TypeDefinitionContainer tc = types.get(typeId);
		if (tc == null) {
			throw new CmisObjectNotFoundException("Type '" + typeId + "' is unknown!");
		}

		return copyTypeDefintion(tc.getTypeDefinition());
	}

	private static TypeDefinition copyTypeDefintion(TypeDefinition type) {
		return WSConverter.convert(WSConverter.convert(type));
	}
}