package com.logicaldoc.cmis;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.AllowableActions;
import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.ExtensionsData;
import org.apache.chemistry.opencmis.commons.data.FailedToDeleteData;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectInFolderList;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.data.ObjectParentData;
import org.apache.chemistry.opencmis.commons.data.PermissionMapping;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.data.PropertyId;
import org.apache.chemistry.opencmis.commons.data.PropertyString;
import org.apache.chemistry.opencmis.commons.data.RepositoryInfo;
import org.apache.chemistry.opencmis.commons.definitions.PermissionDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionContainer;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionList;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.Action;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.CapabilityAcl;
import org.apache.chemistry.opencmis.commons.enums.CapabilityChanges;
import org.apache.chemistry.opencmis.commons.enums.CapabilityContentStreamUpdates;
import org.apache.chemistry.opencmis.commons.enums.CapabilityJoin;
import org.apache.chemistry.opencmis.commons.enums.CapabilityQuery;
import org.apache.chemistry.opencmis.commons.enums.CapabilityRenditions;
import org.apache.chemistry.opencmis.commons.enums.ChangeType;
import org.apache.chemistry.opencmis.commons.enums.CmisVersion;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
import org.apache.chemistry.opencmis.commons.enums.SupportedPermissions;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConstraintException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNameConstraintViolationException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisStorageException;
import org.apache.chemistry.opencmis.commons.impl.MimeTypes;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AclCapabilitiesDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AllowableActionsImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChangeEventInfoDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.CmisExtensionElementImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.FailedToDeleteDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectInFolderDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectInFolderListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectParentDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PermissionDefinitionDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PermissionMappingDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyBooleanImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDateTimeImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDecimalImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyHtmlImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIntegerImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyUriImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RepositoryCapabilitiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RepositoryInfoImpl;
import org.apache.chemistry.opencmis.commons.impl.server.ObjectInfoImpl;
import org.apache.chemistry.opencmis.commons.server.CallContext;
import org.apache.chemistry.opencmis.commons.server.ObjectInfo;
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.folder.FolderHistoryDAO;
import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.searchengine.FulltextSearchOptions;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * LogicalDOC implementation of a CMIS Repository
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5.1
 */
public class LDRepository {

	private static final String OBJECT_IS_NOT_A_DOCUMENT = "Object %s is not a Document!";

	private static final String IS_UNKNOWN = "is unknown!";

	private static final String PROPERTY = "Property '%s' %s";

	private static final String TYPE_S_IS_UNKNOWN = "Type '%s' is unknown!";

	private static final String ID_PREFIX_DOC = "doc.";

	private static final String ID_PREFIX_FLD = "fld.";

	private static final String ID_PREFIX_VER = "ver.";

	private static final String USER_UNKNOWN = "<unknown>";

	private static final String CMIS_READ = "cmis:read";

	private static final String CMIS_WRITE = "cmis:write";

	private static final String CMIS_ALL = "cmis:all";

	private static final int BUFFER_SIZE = 4 * 1024;

	private static final Logger log = LoggerFactory.getLogger(LDRepository.class);

	/** Repository id */
	private final String id;

	private Folder root;

	/** User table */
	private final Map<String, Boolean> userMap;

	/** Repository info */
	private final RepositoryInfoImpl repositoryInfo;

	/** Types */
	private TypeManager types = new TypeManager();

	private UserDAO userDao;

	private FolderDAO folderDao;

	private DocumentDAO documentDao;

	private DocumentHistoryDAO historyDao;

	private FolderHistoryDAO folderHistoryDao;

	private TemplateDAO templateDao;

	private VersionDAO versionDao;

	private DocumentManager documentManager;

	private String sid;

	/**
	 * Constructor.
	 * 
	 * @param sid The Session identifier
	 * @param root root folder
	 */
	public LDRepository(Folder root, String sid) {
		// check root folder
		if (root == null)
			throw new IllegalArgumentException("Invalid root folder!");

		userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		documentManager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		versionDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
		historyDao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
		folderHistoryDao = (FolderHistoryDAO) Context.get().getBean(FolderHistoryDAO.class);

		ContextProperties config = Context.get().getProperties();

		this.sid = sid;
		this.root = root;
		this.id = Long.toString(root.getId());

		// set up user table
		userMap = new HashMap<>();

		// compile repository info
		repositoryInfo = new RepositoryInfoImpl();

		repositoryInfo.setId(id);

		long rootId;
		try {
			rootId = folderDao.findRoot(SessionManager.get().get(sid).getTenantId()).getId();
		} catch (PersistenceException e) {
			throw new IllegalArgumentException("Root folder not found", e);
		}
		if (root.getId() == rootId) {
			repositoryInfo.setName("Main Repository");
			repositoryInfo.setDescription("Main Repository");
		} else {
			repositoryInfo.setName(root.getName());
			repositoryInfo.setDescription(root.getDescription());
		}

		repositoryInfo.setCmisVersion(CmisVersion.CMIS_1_1);
		repositoryInfo.setCmisVersionSupported("1.1");

		repositoryInfo.setProductName("LogicalDOC");
		repositoryInfo.setProductVersion(config.getProperty("product.release"));
		repositoryInfo.setVendorName("LogicalDOC");

		repositoryInfo.setRootFolder(getId(root));

		repositoryInfo.setThinClientUri("");

		RepositoryCapabilitiesImpl capabilities = new RepositoryCapabilitiesImpl();
		capabilities.setCapabilityAcl(CapabilityAcl.DISCOVER);
		capabilities.setAllVersionsSearchable(false);
		capabilities.setCapabilityJoin(CapabilityJoin.NONE);
		capabilities.setSupportsMultifiling(false);
		capabilities.setSupportsUnfiling(false);
		capabilities.setSupportsVersionSpecificFiling(false);
		capabilities.setIsPwcSearchable(Boolean.TRUE);
		capabilities.setIsPwcUpdatable(Boolean.TRUE);
		capabilities.setCapabilityQuery(CapabilityQuery.FULLTEXTONLY);
		capabilities.setCapabilityContentStreamUpdates(CapabilityContentStreamUpdates.PWCONLY);
		capabilities.setSupportsGetDescendants(true);
		capabilities.setSupportsGetFolderTree(true);
		capabilities.setCapabilityRendition(CapabilityRenditions.READ);

		ContextProperties settings = Context.get().getProperties();
		capabilities.setCapabilityChanges(
				"true".equals(settings.getProperty("cmis.changelog")) ? CapabilityChanges.ALL : CapabilityChanges.NONE);

		repositoryInfo.setCapabilities(capabilities);

		AclCapabilitiesDataImpl aclCapability = new AclCapabilitiesDataImpl();
		aclCapability.setSupportedPermissions(SupportedPermissions.BASIC);
		aclCapability.setAclPropagation(AclPropagation.OBJECTONLY);

		// permissions
		List<PermissionDefinition> permissions = new ArrayList<>();
		permissions.add(createPermission(CMIS_READ, "Read"));
		permissions.add(createPermission(CMIS_WRITE, "Write"));
		permissions.add(createPermission(CMIS_ALL, "All"));
		aclCapability.setPermissionDefinitionData(permissions);

		// mapping
		List<PermissionMapping> list = new ArrayList<>();
		list.add(createMapping(PermissionMapping.CAN_CREATE_DOCUMENT_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_CREATE_FOLDER_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_DELETE_CONTENT_DOCUMENT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_DELETE_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_DELETE_TREE_FOLDER, CMIS_ALL));
		list.add(createMapping(PermissionMapping.CAN_GET_ACL_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_ALL_VERSIONS_VERSION_SERIES, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_CHILDREN_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_DESCENDENTS_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_FOLDER_PARENT_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_PARENTS_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_PROPERTIES_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_MOVE_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_MOVE_SOURCE, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_MOVE_TARGET, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_SET_CONTENT_DOCUMENT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_UPDATE_PROPERTIES_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_VIEW_CONTENT_OBJECT, CMIS_READ));

		Map<String, PermissionMapping> map = new LinkedHashMap<>();
		for (PermissionMapping pm : list) {
			map.put(pm.getKey(), pm);
		}
		aclCapability.setPermissionMappingData(map);

		repositoryInfo.setAclCapabilities(aclCapability);

		// ADD LogicalDOC specific extensions sid (session-ID) element
		setupExtensions(repositoryInfo);
	}

	/**
	 * Adds LogicalDOC specific extensions. Here we put the current SID.
	 * 
	 * @param repoInfo The repository info to process
	 */
	private void setupExtensions(RepositoryInfoImpl repoInfo) {

		// Important: use this namespace for the extensions that must be
		// different from the CMIS
		// namespaces
		String ns = "http://www.logicaldoc.com";

		// create a list for the first level of our extension
		List<CmisExtensionElement> extElements = new ArrayList<>();

		// add two leafs to the extension
		extElements.add(new CmisExtensionElementImpl(ns, "sid", null, this.sid));

		// set the extension list
		List<CmisExtensionElement> extensions = new ArrayList<>();
		extensions.add(new CmisExtensionElementImpl(ns, "LogicaldocExtension", null, extElements));

		repoInfo.setExtensions(extensions);
	}

	private static PermissionDefinition createPermission(String id, String description) {
		PermissionDefinitionDataImpl pd = new PermissionDefinitionDataImpl();
		pd.setId(id);
		pd.setDescription(description);

		return pd;
	}

	private static PermissionMapping createMapping(String key, String permission) {
		PermissionMappingDataImpl pm = new PermissionMappingDataImpl();
		pm.setKey(key);
		pm.setPermissions(Collections.singletonList(permission));

		return pm;
	}

	/**
	 * Adds a user to the repository
	 * 
	 * @param user the username
	 * @param readOnly if the user must be read-only
	 */
	public void addUser(String user, boolean readOnly) {
		if ((user == null) || (user.length() == 0))
			return;

		userMap.put(user, readOnly);
	}

	/**
	 * CMIS getRepositoryInfo
	 * 
	 * @param context call context
	 * @param latestChangeLogToken a token that indicates the time stamp of the
	 *        last update
	 * 
	 * @return the repository representation
	 */
	public RepositoryInfo getRepositoryInfo(CallContext context, String latestChangeLogToken) {
		debug("getRepositoryInfo");
		validatePermission(context.getRepositoryId(), context, null);
		if (latestChangeLogToken != null)
			repositoryInfo.setLatestChangeLogToken(latestChangeLogToken);

		return repositoryInfo;
	}

	/**
	 * CMIS getTypesChildren
	 * 
	 * @param context call context
	 * @param typeId identifier of the type
	 * @param includePropertyDefinitions id the property definitions must be
	 *        included
	 * @param maxItems maximum number of children
	 * @param skipCount if the children must be counted
	 * 
	 * @return the list of type definitions
	 */
	public TypeDefinitionList getTypesChildren(CallContext context, String typeId, boolean includePropertyDefinitions,
			BigInteger maxItems, BigInteger skipCount) {
		debug("getTypesChildren " + typeId);
		validatePermission(context.getRepositoryId(), context, null);
		return types.getTypesChildren(context, typeId, includePropertyDefinitions, maxItems, skipCount);
	}

	/**
	 * CMIS getTypeDefinition
	 * 
	 * @param context call context
	 * @param typeId identifier of the type
	 * 
	 * @return the type definition
	 */
	public TypeDefinition getTypeDefinition(CallContext context, String typeId) {
		debug("getTypeDefinition " + typeId);
		validatePermission(context.getRepositoryId(), context, null);

		try {
			return types.getTypeDefinition(context, typeId);
		} catch (Exception t) {
			return (TypeDefinition) catchError(t);
		}
	}

	/**
	 * CMIS getTypesDescendants
	 * 
	 * @param context the call context
	 * @param typeId identifier of the type
	 * @param depth a depth specification
	 * @param includePropertyDefinitions if property definitions must be
	 *        included
	 * 
	 * @return the list of type definitions
	 */
	public List<TypeDefinitionContainer> getTypesDescendants(CallContext context, String typeId, BigInteger depth,
			Boolean includePropertyDefinitions) {
		debug("getTypesDescendants");
		validatePermission(context.getRepositoryId(), context, null);
		return types.getTypesDescendants(context, typeId, depth, includePropertyDefinitions);
	}

	public String createDocumentFromSource(CallContext context, String sourceId, String folderId) {
		debug("createDocumentFromSource");
		validatePermission(folderId, context, Permission.WRITE);

		try {
			Folder target = getFolder(folderId);

			Document doc = (Document) getDocument(sourceId);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.STORED.toString());
			transaction.setUser(getSessionUser());
			transaction.setComment("");

			Document newDoc = documentManager.copyToFolder(doc, target, transaction, true, true);
			return getId(newDoc);

		} catch (Exception t) {
			return (String) catchError(t);
		}
	}

	/**
	 * Create dispatch for AtomPub
	 * 
	 * @param context call context
	 * @param properties the properties
	 * @param folderId identifier of the parent folder
	 * @param contentStream stream of the document to create
	 * @param objectInfos informations
	 * 
	 * @return the newly created object
	 */
	public ObjectData create(CallContext context, Properties properties, String folderId, ContentStream contentStream,
			ObjectInfoHandler objectInfos) {
		debug("create " + folderId);
		validatePermission(folderId, context, Permission.WRITE);

		String typeId = getTypeId(properties);

		TypeDefinition type = types.getType(typeId);
		if (type == null)
			throw new CmisObjectNotFoundException(String.format(TYPE_S_IS_UNKNOWN, typeId));

		String objectId = null;
		if (type.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {
			objectId = createDocument(context, properties, folderId, contentStream);
			return compileObjectType(context, getDocument(objectId), null, false, false, objectInfos);
		} else if (type.getBaseTypeId() == BaseTypeId.CMIS_FOLDER) {
			objectId = createFolder(context, properties, folderId);
			return compileObjectType(context, getFolder(objectId), null, false, false, objectInfos);
		} else {
			throw new CmisObjectNotFoundException("Cannot create object of type '" + typeId + "'!");
		}
	}

	/**
	 * Appends content into existing file
	 * 
	 * @param context call context
	 * @param documentId identifier of the document
	 * @param contentStream binary content of the file to append
	 * @param isLastChunk if this is the last chunk
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the data layer
	 */
	public void appendContent(CallContext context, String documentId, ContentStream contentStream, boolean isLastChunk)
			throws IOException, PersistenceException {
		log.debug("appendContent {}", documentId);
		AbstractDocument doc = getDocument(documentId);

		validatePermission(ID_PREFIX_FLD + doc.getFolder().getId(), context, Permission.WRITE);

		NumberFormat nd = new DecimalFormat("0000000000");

		Storer storer = (Storer) Context.get().getBean(Storer.class);

		File chunksFolder = getChunksFolder(documentId);
		String resourceName = storer.getResourceName(doc.getId(), doc.getFileVersion(), null);
		if (FileUtils.isEmptyDirectory(chunksFolder)) {
			// Copy the current file's content
			File firstChunk = new File("chunk-" + nd.format(1));
			storer.writeToFile(doc.getId(), resourceName, firstChunk);
		}

		int totalChunks = chunksFolder.list().length;
		File currentChunk = new File("chunk-" + nd.format(totalChunks + 1L));
		storer.writeToFile(doc.getId(), resourceName, currentChunk);

		if (isLastChunk) {
			File mergeFile = getMergedContent(chunksFolder);
			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);

			DocumentHistory transaction = new DocumentHistory();
			transaction.setUser(getSessionUser());
			transaction.setSessionId(sid);
			manager.replaceFile(doc.getId(), doc.getFileVersion(), mergeFile, transaction);

			FileUtil.strongDelete(chunksFolder);
		}
	}

	private File getChunksFolder(String documentId) throws IOException {
		return FileUtil.createTempDirectory("cmis" + sid + documentId);
	}

	private File getMergedContent(File chunksDir) throws IOException {
		List<Path> chunks;
		try (Stream<Path> pStream = Files.list(chunksDir.toPath()).sorted()) {
			chunks = pStream.collect(Collectors.toList());
		}

		File merge = new File(chunksDir, "merge");
		boolean created = merge.createNewFile();
		if (!created)
			throw new IOException("Cannot create file " + merge.getAbsolutePath());

		for (Path path : chunks) {
			File chunk = path.toFile();
			Files.write(merge.toPath(), Files.readAllBytes(chunk.toPath()), StandardOpenOption.APPEND);
			FileUtil.strongDelete(chunk);
		}

		return merge;
	}

	/**
	 * CMIS createDocument
	 * 
	 * @param context call context
	 * @param properties the folder's properties
	 * @param folderId identifier of the parent folder
	 * @param contentStream binary content of the file to create
	 * 
	 * @return the new document's identifier
	 */
	public String createDocument(CallContext context, Properties properties, String folderId,
			ContentStream contentStream) {

		log.debug("createDocument {}", folderId);

		validatePermission(folderId, context, Permission.WRITE);

		try {
			// check properties
			if ((properties == null) || (properties.getProperties() == null))
				throw new CmisInvalidArgumentException("Properties must be set!");

			// check type
			String typeId = getTypeId(properties);
			TypeDefinition type = types.getType(typeId);
			if (type == null)
				throw new CmisObjectNotFoundException(String.format(TYPE_S_IS_UNKNOWN, typeId));

			User user = getSessionUser();

			// check the name
			String name = getStringProperty(properties, PropertyIds.NAME);
			if (name == null)
				throw new CmisNameConstraintViolationException("Name is not valid!");

			String fileName = getStringProperty(properties, PropertyIds.CONTENT_STREAM_FILE_NAME);
			if (fileName == null)
				fileName = getStringProperty(properties, "Filename");
			if (fileName == null)
				fileName = name;
			if (!isValidName(fileName))
				throw new CmisNameConstraintViolationException("File name is not valid!");

			Document document = new Document();
			updateDocumentMetadata(document, properties, true);
			document.setTenantId(user.getTenantId());
			document.setFileName(fileName);
			document.setFolder(getFolder(folderId));
			document.setLanguage(user.getLanguage());

			DocumentHistory transaction = new DocumentHistory();
			transaction.setUser(user);
			transaction.setSessionId(sid);

			document = documentManager.create(new BufferedInputStream(contentStream.getStream(), BUFFER_SIZE), document,
					transaction);

			return getId(document);
		} catch (Exception t) {
			return (String) catchError(t);
		}
	}

	/**
	 * CMIS moveObject
	 * 
	 * @param context call context
	 * @param properties the folder's properties
	 * @param folderId identifier of the parent folder
	 * 
	 * @return the created folder's identifier
	 */
	public String createFolder(CallContext context, Properties properties, String folderId) {
		debug("createFolder " + folderId);

		validatePermission(folderId, context, Permission.WRITE);

		try {

			// check properties
			if ((properties == null) || (properties.getProperties() == null))
				throw new CmisInvalidArgumentException("Properties must be set!");

			// check type
			String typeId = getTypeId(properties);
			TypeDefinition type = types.getType(typeId);
			if (type == null)
				throw new CmisObjectNotFoundException(String.format(TYPE_S_IS_UNKNOWN, typeId));

			// check the name
			String name = getStringProperty(properties, PropertyIds.NAME);
			if (!isValidName(name))
				throw new CmisNameConstraintViolationException("Name is not valid.");

			// get parent File
			Folder parent = getFolder(folderId);

			FolderHistory transaction = new FolderHistory();
			transaction.setUser(getSessionUser());
			transaction.setSessionId(sid);

			Folder folder = null;
			Folder newFolder = new Folder(name);

			newFolder.setTenantId(getSessionUser().getTenantId());
			folder = folderDao.create(parent, newFolder, true, transaction);

			return getId(folder);
		} catch (Exception t) {
			return (String) catchError(t);
		}
	}

	/**
	 * CMIS moveObject
	 * 
	 * @param context call context
	 * @param objectId identifier of the file/folder
	 * @param targetFolderId identifier of the target folder
	 * @param objectInfos informations
	 * 
	 * @return the moved object
	 */
	public ObjectData moveObject(CallContext context, Holder<String> objectId, String targetFolderId,
			ObjectInfoHandler objectInfos) {
		debug("moveObject " + objectId + " into " + targetFolderId);

		try {
			if (objectId == null)
				throw new CmisInvalidArgumentException("Id is not valid!");

			validatePermission(targetFolderId, context, Permission.WRITE);
			validatePermission(objectId.getValue(), context, Permission.DOWNLOAD);

			Folder target = getFolder(targetFolderId);

			Object object = getObject(objectId.getValue());

			if (object instanceof Document) {
				DocumentHistory transaction = new DocumentHistory();
				transaction.setUser(getSessionUser());
				transaction.setSessionId(sid);
				documentManager.moveToFolder((Document) object, target, transaction);

				AbstractDocument doc = getDocument(objectId.getValue());
				return compileObjectType(context, doc, null, false, false, objectInfos);
			} else {
				FolderHistory transaction = new FolderHistory();
				transaction.setUser(getSessionUser());
				transaction.setSessionId(sid);
				transaction.setEvent(FolderEvent.MOVED.toString());

				folderDao.move((Folder) object, target, transaction);

				Folder folder = getFolder(objectId.getValue());
				return compileObjectType(context, folder, null, false, false, objectInfos);
			}
		} catch (CmisPermissionDeniedException | PersistenceException e) {
			return (ObjectData) catchError(e);
		}

	}

	private boolean delete(PersistentObject object) {
		try {
			User user = getSessionUser();
			if (object instanceof Folder) {
				Folder folder = (Folder) object;
				FolderHistory transaction = new FolderHistory();
				transaction.setUser(user);
				transaction.setEvent(FolderEvent.DELETED.toString());
				transaction.setSessionId(sid);

				folderDao.delete(folder.getId(), transaction);
			} else {
				Document doc = (Document) object;
				DocumentHistory transaction = new DocumentHistory();
				transaction.setUser(user);
				transaction.setEvent(FolderEvent.DELETED.toString());
				transaction.setSessionId(sid);

				documentDao.delete(doc.getId(), transaction);
			}

			return true;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
			return false;
		}
	}

	public void deleteObjectOrCancelCheckOut(CallContext context, String objectId) {
		// get the file or folder
		PersistentObject object = getObject(objectId);

		if (object instanceof Document) {
			Document doc = (Document) object;
			if (doc.getStatus() != AbstractDocument.DOC_UNLOCKED)
				cancelCheckOut(objectId);
			else
				deleteObject(context, objectId);
		} else
			deleteObject(context, objectId);
	}

	/**
	 * CMIS deleteObject
	 * 
	 * @param context call context
	 * @param objectId identifier of the file/folder
	 */
	public void deleteObject(CallContext context, String objectId) {
		debug("deleteObject " + objectId);
		validatePermission(objectId, context, Permission.DELETE);

		try {
			// get the file or folder
			PersistentObject object = getObject(objectId);

			if (object instanceof Folder) {
				Folder folder = (Folder) object;
				List<Document> docs = documentDao.findByFolder(folder.getId(), 2);
				List<Folder> folders = folderDao.findByParentId(folder.getId());

				// check if it is a folder and if it is empty
				if (!docs.isEmpty() || !folders.isEmpty())
					throw new CmisConstraintException(String.format("Folder %s is not empty!", objectId));
			}

			if (!delete(object))
				throw new CmisStorageException("Deletion failed!");
		} catch (Exception t) {
			catchError(t);
		}
	}

	/**
	 * CMIS deleteTree
	 * 
	 * @param context call context
	 * @param folderId identifier of the folder
	 * @param continueOnFailure if the execution must continue even in presence
	 *        of an issue
	 * 
	 * @return informations about the failure
	 */
	public FailedToDeleteData deleteTree(CallContext context, String folderId, Boolean continueOnFailure) {
		debug("deleteTree " + folderId);
		validatePermission(folderId, context, Permission.DELETE);

		boolean cof = continueOnFailure != null && continueOnFailure.booleanValue();

		// get the document or folder
		PersistentObject object = getObject(folderId);

		FailedToDeleteDataImpl result = new FailedToDeleteDataImpl();
		result.setIds(new ArrayList<>());

		try {
			if (object instanceof Folder) {
				Folder folder = (Folder) object;
				deleteFolder(folder, cof, result);
			} else {
				Document doc = (Document) object;
				DocumentHistory transaction = new DocumentHistory();
				transaction.setUser(getSessionUser());
				transaction.setEvent(FolderEvent.DELETED.toString());
				transaction.setSessionId(sid);

				documentDao.delete(doc.getId(), transaction);
			}
		} catch (Exception t) {
			log.error(t.getMessage(), t);
			throw new CmisStorageException("Deletion failed!");
		}
		return result;
	}

	/**
	 * CMIS updateProperties
	 * 
	 * @param context the call context
	 * @param objectId identifier of the object
	 * @param properties the properties
	 * @param objectInfos informations
	 * 
	 * @return the updated object
	 */
	public ObjectData updateProperties(CallContext context, Holder<String> objectId, Properties properties,
			ObjectInfoHandler objectInfos) {
		debug("updateProperties " + objectId);
		validatePermission(objectId.getValue(), context, Permission.WRITE);

		try {
			// get the document or folder
			PersistentObject object = getObject(objectId.getValue());
			log.debug("object.getId() {}", object.getId());

			// get old properties
			Properties oldProperties = compileProperties(object, null, new ObjectInfoImpl());

			update(object, oldProperties, properties);

			return compileObjectType(context, object, null, false, false, objectInfos);
		} catch (PersistenceException e) {
			return (ObjectData) catchError(e);
		}
	}

	public ObjectInfo getObjectInfo(String objectId, ObjectInfoHandler handler) {
		debug("getObjectInfo " + objectId);
		validatePermission(objectId, null, null);

		try {
			// check id
			if (objectId == null)
				throw new CmisInvalidArgumentException("Object Id must be set.");

			ObjectInfoImpl info = new ObjectInfoImpl();
			PersistentObject object = getObject(objectId);
			compileProperties(object, null, info);
			ObjectData data = compileObjectType(null, object, null, true, true, handler);
			info.setObject(data);
			return info;
		} catch (Exception t) {
			log.warn("Not able to retrieve object {}", objectId);
			return (ObjectInfo) catchError(t);
		}
	}

	public void checkOut(Holder<String> objectId, Holder<Boolean> contentCopied) {
		log.debug("checkOut {}", objectId);
		validatePermission(objectId.getValue(), null, Permission.WRITE);

		try {
			// get the document
			PersistentObject object = getObject(objectId.getValue());

			if (!(object instanceof Document))
				throw new CmisObjectNotFoundException(String.format(OBJECT_IS_NOT_A_DOCUMENT, objectId.getValue()));

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
			transaction.setComment("");
			transaction.setUser(getSessionUser());

			documentManager.checkout(object.getId(), transaction);
			objectId.setValue(getId(object));
			if (contentCopied != null)
				contentCopied.setValue(Boolean.TRUE);
		} catch (Exception t) {
			catchError(t);
		}
	}

	public void cancelCheckOut(String objectId) {
		debug("cancelCheckOut " + objectId);
		validatePermission(objectId, null, Permission.WRITE);

		try {
			// get the document
			PersistentObject object = getObject(objectId);

			if (!(object instanceof Document))
				throw new CmisObjectNotFoundException(String.format(OBJECT_IS_NOT_A_DOCUMENT, objectId));

			Document doc = (Document) object;

			if (doc.getStatus() == AbstractDocument.DOC_CHECKED_OUT
					&& ((getSessionUser().getId() != doc.getLockUserId())
							&& (!getSessionUser().isMemberOf(Group.GROUP_ADMIN))))
				throw new CmisPermissionDeniedException("You can't change the checkout status on this object!");

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.UNLOCKED.toString());
			transaction.setComment("");
			transaction.setUser(getSessionUser());

			documentDao.initialize(doc);
			doc.setStatus(AbstractDocument.DOC_UNLOCKED);
			documentDao.store(doc, transaction);
		} catch (Exception t) {
			catchError(t);
		}
	}

	public void checkIn(Holder<String> objectId, Boolean major, ContentStream contentStream, Properties properties,
			String checkinComment) {
		log.debug("checkin {}", objectId);
		validatePermission(objectId.getValue(), null, Permission.WRITE);

		try {
			PersistentObject object = getObject(objectId.getValue());

			if (!(object instanceof Document))
				throw new CmisObjectNotFoundException(String.format(OBJECT_IS_NOT_A_DOCUMENT, objectId.getValue()));

			Document doc = (Document) object;

			if (doc.getStatus() == AbstractDocument.DOC_CHECKED_OUT
					&& ((getSessionUser().getId() != doc.getLockUserId())
							&& (!getSessionUser().isMemberOf(Group.GROUP_ADMIN)))) {
				throw new CmisPermissionDeniedException(
						String.format("You can't do a checkin on object %s!", objectId.getValue()));
			}

			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.CHECKEDIN.toString());
			transaction.setUser(getSessionUser());
			transaction.setComment(checkinComment);

			if (properties != null) {
				updateDocumentMetadata(doc, properties, false);
			}

			documentManager.checkin(doc.getId(), contentStream.getStream(), doc.getFileName(), major, null,
					transaction);
		} catch (Exception t) {
			catchError(t);
		}
	}

	/**
	 * CMIS getObject
	 * 
	 * @param context the call context
	 * @param objectId identifier of the file/foler
	 * @param versionServicesId version specification
	 * @param filter optional filter
	 * @param includeAllowableActions if the allowable actions must be included
	 * @param includeAcl if the ACL must be included
	 * @param objectInfos informations
	 * 
	 * @return the file/folder
	 */
	public ObjectData getObject(CallContext context, String objectId, String versionServicesId, String filter,
			Boolean includeAllowableActions, Boolean includeAcl, ObjectInfoHandler objectInfos) {
		debug("getObject " + objectId);
		try {
			validatePermission(objectId, context, null);

			if (objectId == null) {
				// this works only because there are no versions in a file
				// system
				// and the object id and version series id are the same
				objectId = versionServicesId;
			}

			PersistentObject obj = getObject(objectId);

			// set defaults if values not set
			boolean iaa = includeAllowableActions != null && includeAllowableActions.booleanValue();
			boolean iacl = includeAcl != null && includeAcl.booleanValue();

			// split filter
			Set<String> filterCollection = splitFilter(filter);

			// gather properties
			return compileObjectType(context, obj, filterCollection, iaa, iacl, objectInfos);
		} catch (Exception t) {
			return (ObjectData) catchError(t);
		}
	}

	/**
	 * CMIS getObjectByPath
	 * 
	 * @param context the call context
	 * @param path path of the file/folder
	 * @param filter the filter(optional)
	 * @param includeAllowableActions if the allowable actions must be included
	 * @param includeRelationships if the relationship must be included
	 * @param renditionFilter the rendition filter(optional)
	 * @param includePolicyIds if the policies must be included
	 * @param includeAcl if the ACL must be included
	 * @param extension the extension
	 * 
	 * @return the file/folder at the given path
	 */
	public ObjectData getObjectByPath(CallContext context, String path, String filter, Boolean includeAllowableActions,
			IncludeRelationships includeRelationships, String renditionFilter, Boolean includePolicyIds,
			Boolean includeAcl, ExtensionsData extension) {

		debug("getObjectByPath " + path);

		ObjectData out = null;

		try {
			String fullPath = path;
			if (fullPath.endsWith("/") && !"/".equals(fullPath))
				fullPath = fullPath.substring(0, fullPath.length() - 1);
			else if (fullPath.endsWith("/."))
				fullPath = fullPath.substring(0, fullPath.length() - 2);

			debug("using normalized path " + fullPath);

			// Try to check if the path is a folder
			Folder folder = folderDao.findByPathExtended(fullPath, getSessionUser().getTenantId());

			if (folder != null) {
				out = getObject(context, ID_PREFIX_FLD + Long.toString(folder.getId()), null, filter,
						includeAllowableActions, includeAcl, null);
			} else {
				// Not a folder, probably a file
				String parentPath = fullPath.substring(0, fullPath.lastIndexOf('/'));
				folder = folderDao.findByPathExtended(parentPath, getSessionUser().getTenantId());

				if (folder != null) {
					String fileName = fullPath.substring(fullPath.lastIndexOf('/') + 1);

					List<Document> docs = documentDao.findByFileNameAndParentFolderId(folder.getId(), fileName, null,
							getSessionUser().getTenantId(), null);

					if (docs != null && (!docs.isEmpty())) {
						out = getObject(context, ID_PREFIX_DOC + Long.toString(docs.get(0).getId()), null, filter,
								includeAllowableActions, includeAcl, null);
					}
				}
			}

			if (out == null)
				throw new CmisObjectNotFoundException("Object not found!");

		} catch (Exception t) {
			return (ObjectData) catchError(t);
		}

		return out;
	}

	/**
	 * CMIS getAllowableActions
	 * 
	 * @param context the call context
	 * @param objectId identifier of the file/folder
	 * 
	 * @return the allowable actions
	 */
	public AllowableActions getAllowableActions(CallContext context, String objectId) {
		debug("getAllowableActions");
		return compileAllowableActions(getObject(objectId));
	}

	/**
	 * CMIS getACL
	 * 
	 * @param context the call context
	 * @param objectId identifier of the file/folder
	 * 
	 * @return the ACL
	 */
	public Acl getAcl(CallContext context, String objectId) {
		debug("getAcl");
		validatePermission(objectId, context, null);

		return compileAcl(getObject(objectId));
	}

	/**
	 * CMIS getContentStream
	 * 
	 * @param context the call context
	 * @param objectId identifier of the file/folder
	 * @param offset not supported, leave null
	 * @param length not supported, leave null
	 * 
	 * @return the stream
	 */
	public ContentStream getContentStream(CallContext context, String objectId, BigInteger offset, BigInteger length) {
		debug("getContentStream " + objectId + " offset:" + offset + " length:" + length);

		validatePermission(objectId, context, Permission.DOWNLOAD);

		try {
			if ((offset != null) || (length != null))
				throw new CmisInvalidArgumentException("Offset and Length are not supported!");

			AbstractDocument doc = getDocument(objectId);

			InputStream stream = null;
			Storer storer = (Storer) Context.get().getBean(Storer.class);
			InputStream is = null;
			if (doc instanceof Document) {
				is = storer.getStream(doc.getId(), storer.getResourceName((Document) doc, null, null));
			} else {
				Version v = (Version) doc;
				is = storer.getStream(v.getDocId(), storer.getResourceName(v.getDocId(), v.getFileVersion(), null));
			}
			stream = new BufferedInputStream(is, BUFFER_SIZE);

			// Create the document history event
			DocumentHistory transaction = new DocumentHistory();
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.DOWNLOADED.toString());
			transaction.setComment("");
			transaction.setUser(getSessionUser());
			transaction.setDocId(doc.getId());
			transaction.setVersion(doc.getVersion());
			transaction.setFileVersion(doc.getFileVersion());
			transaction.setFilename(doc.getFileName());
			transaction.setFileSize(doc.getFileSize());
			transaction.setNotified(0);
			if (doc instanceof Document)
				transaction.setDocument((Document) doc);

			if (doc instanceof Document) {
				transaction.setFolderId(doc.getFolder().getId());
				transaction.setPath(folderDao.computePathExtended(doc.getFolder().getId()));
			} else {
				transaction.setFolderId(((Version) doc).getFolderId());
				transaction.setPath(folderDao.computePathExtended(((Version) doc).getFolderId()));
			}

			saveHistory(transaction);

			// compile data
			ContentStreamImpl result = new ContentStreamImpl();
			result.setFileName(doc.getFileName());
			result.setMimeType(MimeTypes.getMIMEType(doc.getFileName()));
			result.setLength(BigInteger.valueOf(doc.getFileSize()));
			result.setStream(stream);

			return result;
		} catch (Exception t) {
			return (ContentStream) catchError(t);
		}
	}

	private void saveHistory(DocumentHistory transaction) {
		try {
			DocumentHistoryDAO historyDAO = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
			historyDAO.store(transaction);
		} catch (PersistenceException t) {
			log.warn(t.getMessage(), t);
		}
	}

	private Object catchError(Throwable t) {
		if (t instanceof CmisObjectNotFoundException)
			log.debug(t.getMessage());
		else if (t instanceof IllegalArgumentException)
			log.warn(t.getMessage());
		else
			log.error(t.getMessage(), t);

		if (t instanceof CmisBaseException)
			throw (CmisBaseException) t;
		else
			throw new CmisStorageException("CMIS Error!", t);
	}

	/**
	 * CMIS getChildren
	 * 
	 * @param context the call context
	 * @param folderId identifier of the folder
	 * @param filter optional filter
	 * @param includeAllowableActions if the allowable actions must be included
	 * @param includePathSegment if the path segment
	 * @param maxItems maximum number of items
	 * @param skipCount skip the count of children
	 * @param objectInfos informations
	 * 
	 * @return the list of children
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public ObjectInFolderList getChildren(CallContext context, String folderId, String filter,
			boolean includeAllowableActions, boolean includePathSegment, int maxItems, int skipCount,
			ObjectInfoHandler objectInfos) throws PersistenceException {
		log.debug("getChildren folderId: {}", folderId);
		validatePermission(folderId, context, null);

		// split filter
		Set<String> filterCollection = splitFilter(filter);

		// get the folder
		Folder folder = getFolder(folderId);

		// set object info of the the folder
		if (context.isObjectInfoRequired()) {
			compileObjectType(context, folder, null, false, false, objectInfos);
		}

		// prepare result
		ObjectInFolderListImpl result = new ObjectInFolderListImpl();
		result.setObjects(new ArrayList<>());
		result.setHasMoreItems(false);

		User user = userDao.findByUsername(context.getUsername());
		long userId = user.getId();

		// Filter the visible folders only
		List<Folder> visibleChildFolders = folderDao.findChildren(folder.getId(), userId).stream()
				.filter(f -> f.getHidden() == 0).collect(Collectors.toList());

		// Skip the first skipCount elements
		List<Folder> childFolders = visibleChildFolders.stream().skip(skipCount).collect(Collectors.toList());

		// iterate through child folders
		for (Folder child : childFolders) {
			if (result.getObjects().size() >= maxItems) {
				result.setHasMoreItems(true);
				break;
			}

			// build and add child object
			buildAndAddChildFolder(child, result, includeAllowableActions, includePathSegment, objectInfos,
					filterCollection, context);
		}

		// Adjust the skipCount to consider those already skipped folders
		skipCount = visibleChildFolders.size() < skipCount ? visibleChildFolders.size() : 0;

		List<Document> childDocuments = documentDao.findByFolder(folder.getId(), null).stream().skip(skipCount)
				.collect(Collectors.toList());

		// iterate through child documents
		for (Document child : childDocuments) {
			if (result.getObjects().size() >= maxItems) {
				result.setHasMoreItems(true);
				break;
			}

			// build and add child object
			buildAndAddChildDocument(child, result, includeAllowableActions, includePathSegment, objectInfos,
					filterCollection, context);
		}

		result.setNumItems(BigInteger.valueOf(result.getObjects().size()));

		return result;

	}

	private void buildAndAddChildDocument(Document child, ObjectInFolderListImpl result,
			boolean includeAllowableActions, boolean includePathSegment, ObjectInfoHandler objectInfos,
			Set<String> filterCollection, CallContext context) {
		ObjectInFolderDataImpl objectInFolder = new ObjectInFolderDataImpl();
		objectInFolder.setObject(
				compileObjectType(context, child, filterCollection, includeAllowableActions, false, objectInfos));
		if (includePathSegment)
			objectInFolder.setPathSegment(child.getFileName());
		result.getObjects().add(objectInFolder);
	}

	private void buildAndAddChildFolder(Folder child, ObjectInFolderListImpl result, boolean includeAllowableActions,
			boolean includePathSegment, ObjectInfoHandler objectInfos, Set<String> filterCollection,
			CallContext context) {
		ObjectInFolderDataImpl objectInFolder = new ObjectInFolderDataImpl();
		objectInFolder.setObject(
				compileObjectType(context, child, filterCollection, includeAllowableActions, false, objectInfos));
		if (includePathSegment)
			objectInFolder.setPathSegment(child.getName());
		result.getObjects().add(objectInFolder);
	}

	private Folder getFolder(String folderId) {
		PersistentObject object = getObject(folderId);
		if (!(object instanceof Folder))
			throw new CmisObjectNotFoundException("Not a folder!");
		return (Folder) object;
	}

	private AbstractDocument getDocument(String documentId) {
		PersistentObject object = getObject(documentId);
		if (!(object instanceof AbstractDocument))
			throw new CmisObjectNotFoundException("Not a document!");
		return (AbstractDocument) object;
	}

	/**
	 * CMIS getFolderParent
	 * 
	 * @param context the call context
	 * @param folderId identifier of the folder
	 * 
	 * @return the parent object
	 */
	public ObjectData getFolderParent(CallContext context, String folderId) {
		Folder folder = getFolder(folderId);
		folder = getFolder(ID_PREFIX_FLD + folder.getParentId());
		return compileObjectType(context, folder, null, false, false, null);
	}

	/**
	 * CMIS getObjectParents
	 * 
	 * @param context the call context
	 * @param objectId identifier of the file/folder
	 * @param filter an optional filter
	 * @param includeAllowableActions if the allowable actions must be included
	 * @param includeRelativePathSegment if the path segment must be included
	 * @param objectInfos informations
	 * 
	 * @return list of parents
	 */
	@SuppressWarnings("unchecked")
	public List<ObjectParentData> getObjectParents(CallContext context, String objectId, String filter,
			Boolean includeAllowableActions, Boolean includeRelativePathSegment, ObjectInfoHandler objectInfos) {
		debug("getObjectParents " + objectId + " " + filter);
		validatePermission(objectId, context, null);

		try {
			// split filter
			Set<String> filterCollection = splitFilter(filter);

			// set defaults if values not set
			boolean iaa = includeAllowableActions != null && includeAllowableActions.booleanValue();
			boolean irps = includeRelativePathSegment != null && includeRelativePathSegment.booleanValue();

			// get the file or folder
			PersistentObject object = getObject(objectId);

			// don't climb above the root folder
			if (root.equals(object))
				return Collections.emptyList();

			// set object info of the the object
			if (context.isObjectInfoRequired())
				compileObjectType(context, object, null, false, false, objectInfos);

			Folder parent;
			if (object instanceof AbstractDocument)
				parent = ((AbstractDocument) object).getFolder();
			else
				parent = folderDao.findFolder(((Folder) object).getParentId());

			// get parent folder
			ObjectData obj = compileObjectType(context, parent, filterCollection, iaa, false, objectInfos);

			ObjectParentDataImpl result = new ObjectParentDataImpl();
			result.setObject(obj);
			if (irps) {
				if (object instanceof Document)
					result.setRelativePathSegment(((Document) object).getFileName());
				else
					result.setRelativePathSegment(((Folder) object).getName());
			}

			return Collections.singletonList((ObjectParentData) result);
		} catch (Exception t) {
			return (List<ObjectParentData>) catchError(t);
		}
	}

	public ObjectList query(String statement, int maxItems) {

		// As expression we will use the WHERE clause as is
		final String where = "where";
		String whereClause = StringUtils.removeStartIgnoreCase(statement, where);
		if (whereClause.toLowerCase().contains(where))
			whereClause = whereClause.substring(whereClause.toLowerCase().indexOf(where) + 6).trim();

		log.debug("where: {}", whereClause);

		/**
		 * Try to detect if the request comes from LogicalDOC Mobile
		 */
		boolean fileNameSearch = whereClause.toLowerCase().contains("cmis:name");
		log.debug("fileNameSearch: {}", fileNameSearch);

		Long parentFolderID = null;

		// Search in Tree
		if (statement.toLowerCase().contains("in_tree")) {
			String folderId = StringUtils.substringBetween(statement, "('fld.", "')");
			parentFolderID = Long.parseLong(folderId);
		}

		// Empty list of ObjectData
		List<ObjectData> list = new ArrayList<>();
		boolean hasMoreItems = false;

		// Create the filter
		Set<String> filter = buildQueryFilter(statement);

		// Performs Full-text search
		if (!fileNameSearch) {
			hasMoreItems = doFulltextSearch(statement, whereClause, parentFolderID, list, filter, maxItems);
		} else {
			try {
				doFilenameSearch(statement, list, filter, maxItems);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}

		ObjectListImpl objList = new ObjectListImpl();
		objList.setObjects(list);
		objList.setNumItems(BigInteger.valueOf(list.size()));
		objList.setHasMoreItems(hasMoreItems);

		return objList;
	}

	private void doFilenameSearch(String statement, List<ObjectData> list, Set<String> filter, int maxItems)
			throws PersistenceException {
		// Performs file name search
		User user = getSessionUser();

		String expr = StringUtils.substringBetween(statement, "'", "'");
		String filename = expr;
		log.debug("filename: {}", filename);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Document> docs = docDao.findByFileNameAndParentFolderId(null, filename, null, getSessionUser().getId(),
				maxItems);

		for (int i = 0; i < docs.size(); i++) {
			// Check permissions on the documents found
			try {
				checkReadEnable(user, docs.get(i).getFolder().getId());
				checkPublished(user, docs.get(i));
			} catch (Exception e) {
				continue;
			}
			docDao.initialize(docs.get(i));

			// filtro i risultati (lasciando solo le colonne richieste)
			ObjectData result = compileObjectType(null, docs.get(i), filter, false, false, null);
			list.add(result);
		}
	}

	private boolean doFulltextSearch(String statement, String whereExpression, Long parentFolderID,
			List<ObjectData> results, Set<String> filter, int maxItems) {
		boolean hasMoreItems;
		FulltextSearchOptions opt = buildFulltextSearchOptions(statement, parentFolderID, maxItems);

		// Now detect if the search must be applied to specific fields
		final String ldocPrefix = "ldoc:";
		if (whereExpression.contains("cmis:") || whereExpression.contains(ldocPrefix)) {
			List<String> fields = new ArrayList<>();
			Pattern p = Pattern.compile("[(cmis),(ldoc)]+:\\w+");
			Matcher m = p.matcher(whereExpression);
			while (m.find()) {
				String field = m.group();
				if (field.startsWith(ldocPrefix))
					field = field.substring(ldocPrefix.length());

				log.debug("field: {}", field);
				fields.add(field);
			}

			opt.setFields(fields.toArray(new String[0]));
		}

		// Execute the search
		log.debug("opt: {}", opt);
		Search search = Search.get(opt);
		log.debug("search: {}", search);
		List<Hit> hits = new ArrayList<>();
		try {
			hits = search.search();
		} catch (SearchException e1) {
			log.error(e1.getMessage(), e1);
		}
		log.debug("hits.size(): {}", hits.size());

		// Iterate through the list of results
		for (Hit hit : hits) {
			try {
				// filtro i risultati (lasciando solo le colonne
				// richieste)
				ObjectData result = compileObjectType(null, hit, filter, false, false, null);
				results.add(result);
			} catch (Exception t) {
				log.error("CMIS Exception populating data structure", t);
			}
		}
		hasMoreItems = search.getEstimatedHitsNumber() > maxItems;
		return hasMoreItems;
	}

	private Set<String> buildQueryFilter(String statement) {
		Set<String> filter = null;
		try {
			// Parse the select list and compile a filter
			String query = statement;
			if (query.toLowerCase().startsWith("select")) {
				query = query.substring(6).trim();
				int fromIndex = query.toLowerCase().indexOf("from", 0);
				if (fromIndex > 0) {
					query = query.substring(0, fromIndex).trim();
				}
				if (!StringUtils.isEmpty(query) && !"*".equals(query)) {
					filter = new HashSet<>();
					StringTokenizer st = new StringTokenizer(query, ",", false);
					while (st.hasMoreTokens())
						filter.add(st.nextToken().trim());
				}
			}
		} catch (Exception e1) {
			log.error("CMIS Exception creating filter", e1);
		}
		return filter;
	}

	private FulltextSearchOptions buildFulltextSearchOptions(String statement, Long parentFolderID, int maxItems) {
		log.debug("Performs Full-text search");
		String expr = StringUtils.substringBetween(statement, "'%", "%'");
		if (StringUtils.isEmpty(expr))
			expr = StringUtils.substringBetween(statement, "('", "')");
		if (StringUtils.isEmpty(expr))
			expr = StringUtils.substringBetween(statement, "'", "'");

		log.debug("expr: {}", expr);

		// Prepare the search options
		FulltextSearchOptions opt = new FulltextSearchOptions();
		opt.setMaxHits(maxItems);

		User user = getSessionUser();
		opt.setUserId(user.getId());
		log.debug("user.getLanguage(): {}", user.getLanguage());
		opt.setExpressionLanguage(user.getLanguage());

		// Check to search in Tree
		if (parentFolderID != null) {
			log.debug("parentFolderID != null");
			opt.setFolderId(parentFolderID);
			opt.setSearchInSubPath(true);
		}

		opt.setExpression(expr);
		return opt;
	}

	// --- helper methods ---

	private void checkPublished(User user, Document doc) throws PermissionException {
		if (!user.isMemberOf(Group.GROUP_ADMIN) && !user.isMemberOf("publisher") && !doc.isPublishing())
			throw new PermissionException("Document not published");
	}

	private void checkReadEnable(User user, long folderId) throws PermissionException, PersistenceException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isReadEnabled(folderId, user.getId())) {
			String message = "User " + user.getUsername() + " doesn't have read permission on folder " + folderId;
			log.error(message);
			throw new PermissionException(message);
		}
	}

	/**
	 * Removes a folder and its content
	 * 
	 * @param folder the folder to delete
	 * @param continueOnFailure if the execution must be finished even in
	 *        presence of an error
	 * @param ftd informations about the failure
	 * 
	 * @return if the execution has been done successfully
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private boolean deleteFolder(Folder folder, boolean continueOnFailure, FailedToDeleteDataImpl ftd)
			throws PersistenceException {
		boolean success = true;
		for (Document doc : documentDao.findByFolder(folder.getId(), null)) {
			if (!delete(doc)) {
				ftd.getIds().add(getId(doc));
				if (!continueOnFailure) {
					return false;
				}
				success = false;
			}
		}

		for (Folder fld : folderDao.findChildren(folder.getId(), null)) {
			if (!deleteFolder(fld, continueOnFailure, ftd)) {
				if (!continueOnFailure) {
					return false;
				}
				success = false;
			}
		}

		if (!delete(folder)) {
			ftd.getIds().add(getId(folder));
			success = false;
		}

		return success;
	}

	/**
	 * Checks if the given name is valid for a file system.
	 * 
	 * @param name the name to check
	 * 
	 * @return <code>true</code> if the name is valid, <code>false</code>
	 *         otherwise
	 */
	private static boolean isValidName(String name) {
		return !(name == null || name.length() == 0 || (name.indexOf('/') != -1));
	}

	/**
	 * Compiles an object type object from a document or folder
	 * 
	 * @param context the call context
	 * @param object the persistent object
	 * @param filter optional filter
	 * @param includeAllowableActions if the allowable actions must be included
	 * @param includeAcl if the ACL must be included
	 * @param objectInfos informations
	 * 
	 * @return the object
	 */
	private ObjectData compileObjectType(CallContext context, PersistentObject object, Set<String> filter,
			boolean includeAllowableActions, boolean includeAcl, ObjectInfoHandler objectInfos) {
		debug("compileObjectType");
		ObjectDataImpl result = new ObjectDataImpl();
		ObjectInfoImpl objectInfo = new ObjectInfoImpl();

		result.setProperties(compileProperties(object, filter, objectInfo));

		if (includeAllowableActions) {
			result.setAllowableActions(compileAllowableActions(object));
		}

		if (includeAcl) {
			result.setAcl(compileAcl(object));
			result.setIsExactAcl(true);
		}

		if ((context != null) && context.isObjectInfoRequired() && (objectInfos != null)) {
			objectInfo.setObject(result);
			objectInfos.addObjectInfo(objectInfo);
		}

		return result;
	}

	/**
	 * Gathers all base properties of a document or folder
	 * 
	 * @param object the persistent object(Folder or Document)
	 * @param orgfilter list of optional filters
	 * @param objectInfo informations
	 * 
	 * @return the compiled properties
	 */
	private Properties compileProperties(PersistentObject object, Set<String> orgfilter, ObjectInfoImpl objectInfo) {
		debug("compileProperties");

		if (object == null)
			throw new IllegalArgumentException("Object must not be null");

		// copy filter
		Set<String> filter = (orgfilter == null ? null : new HashSet<>(orgfilter));

		// find base type
		String typeId = null;

		if (object instanceof Folder) {
			typeId = TypeManager.FOLDER_TYPE_ID;
			objectInfo.setTypeId(typeId);
			if (((Folder) object).getType() == 1) {
				typeId = TypeManager.WORKSPACE_TYPE_ID;
				objectInfo.setTypeId(typeId);
			}
			objectInfo.setBaseType(BaseTypeId.CMIS_FOLDER);
			objectInfo.setContentType(null);
			objectInfo.setFileName(null);
			objectInfo.setHasAcl(true);
			objectInfo.setHasContent(false);
			objectInfo.setVersionSeriesId(null);
			objectInfo.setIsCurrentVersion(true);
			objectInfo.setRelationshipSourceIds(null);
			objectInfo.setRelationshipTargetIds(null);
			objectInfo.setRenditionInfos(null);
			objectInfo.setSupportsDescendants(true);
			objectInfo.setSupportsFolderTree(true);
			objectInfo.setSupportsPolicies(false);
			objectInfo.setSupportsRelationships(false);
			objectInfo.setWorkingCopyId(null);
			objectInfo.setWorkingCopyOriginalId(null);
		} else {
			typeId = TypeManager.DOCUMENT_TYPE_ID;
			objectInfo.setTypeId(typeId);
			objectInfo.setBaseType(BaseTypeId.CMIS_DOCUMENT);
			objectInfo.setHasAcl(true);
			objectInfo.setHasContent(true);
			objectInfo.setHasParent(true);
			objectInfo.setVersionSeriesId(getId(object));
			objectInfo.setIsCurrentVersion(true);
			objectInfo.setRelationshipSourceIds(null);
			objectInfo.setRelationshipTargetIds(null);
			objectInfo.setRenditionInfos(null);
			objectInfo.setSupportsDescendants(false);
			objectInfo.setSupportsFolderTree(false);
			objectInfo.setSupportsPolicies(false);
			objectInfo.setSupportsRelationships(false);
			objectInfo.setWorkingCopyId(null);
			objectInfo.setWorkingCopyOriginalId(null);
		}

		// let's do it

		try {
			PropertiesImpl result = new PropertiesImpl();

			// id
			String objId = getId(object);
			addPropertyId(result, typeId, filter, PropertyIds.OBJECT_ID, objId);
			objectInfo.setId(objId);

			// name
			String name = "";
			if (object instanceof Folder)
				name = ((Folder) object).getName();
			else
				name = ((AbstractDocument) object).getFileName();

			addPropertyString(result, typeId, filter, PropertyIds.NAME, name);
			objectInfo.setName(name);

			// modification date
			GregorianCalendar lastModified = millisToCalendar(object.getLastModified().getTime());
			addPropertyDateTime(result, typeId, filter, PropertyIds.LAST_MODIFICATION_DATE, lastModified);
			objectInfo.setLastModificationDate(lastModified);
			// change token - always null
			addPropertyString(result, typeId, filter, PropertyIds.CHANGE_TOKEN, null);

			// directory or file
			if (object instanceof Folder) {
				// Object is a Folder
				Folder folder = ((Folder) object);
				compileFolderProperties(folder, objectInfo, filter, typeId, result);
			} else {
				// Object is a Document or Version
				AbstractDocument document = (AbstractDocument) object;
				compileDocumentOrVersionProperties(document, objectInfo, filter, typeId, result);
			}

			return result;
		} catch (PersistenceException e) {
			throw new CmisRuntimeException(e.getMessage(), e);
		}
	}

	private void compileDocumentOrVersionProperties(AbstractDocument doc, ObjectInfoImpl objectInfo, Set<String> filter,
			String typeId, PropertiesImpl result) throws PersistenceException {
		GregorianCalendar creation = millisToCalendar(doc.getCreation().getTime());

		// created and modified by
		addPropertyString(result, typeId, filter, PropertyIds.CREATED_BY, doc.getCreator());
		addPropertyString(result, typeId, filter, PropertyIds.LAST_MODIFIED_BY, doc.getPublisher());
		objectInfo.setCreatedBy(doc.getPublisher());

		addPropertyDateTime(result, typeId, filter, PropertyIds.CREATION_DATE, creation);
		objectInfo.setCreationDate(creation);

		// base type and type name
		addPropertyId(result, typeId, filter, PropertyIds.BASE_TYPE_ID, BaseTypeId.CMIS_DOCUMENT.value());
		addPropertyId(result, typeId, filter, PropertyIds.OBJECT_TYPE_ID, TypeManager.DOCUMENT_TYPE_ID);

		// file properties
		if (doc instanceof Document) {
			addPropertyBoolean(result, typeId, filter, PropertyIds.IS_LATEST_VERSION, true);
			addPropertyBoolean(result, typeId, filter, PropertyIds.IS_MAJOR_VERSION,
					doc.getVersion() == null || doc.getVersion().endsWith(".0"));
		} else {
			Version ver = (Version) doc;
			AbstractDocument d = getDocument(ID_PREFIX_DOC + ver.getDocId());

			addPropertyBoolean(result, typeId, filter, PropertyIds.IS_LATEST_VERSION,
					d.getVersion().equals(ver.getVersion()));
			addPropertyBoolean(result, typeId, filter, PropertyIds.IS_MAJOR_VERSION, ver.getVersion().endsWith(".0"));
		}
		addPropertyString(result, typeId, filter, PropertyIds.VERSION_LABEL, doc.getVersion());
		addPropertyId(result, typeId, filter, PropertyIds.VERSION_SERIES_ID, getId(doc));
		if (doc.getStatus() != AbstractDocument.DOC_CHECKED_OUT) {
			addPropertyBoolean(result, typeId, filter, PropertyIds.IS_VERSION_SERIES_CHECKED_OUT, false);
			addPropertyString(result, typeId, filter, PropertyIds.VERSION_SERIES_CHECKED_OUT_BY, null);
			addPropertyString(result, typeId, filter, PropertyIds.VERSION_SERIES_CHECKED_OUT_ID, null);
		} else {
			User checkoutUser = null;
			if (doc.getLockUserId() != null)
				checkoutUser = userDao.findById(doc.getLockUserId());
			addPropertyBoolean(result, typeId, filter, PropertyIds.IS_VERSION_SERIES_CHECKED_OUT, true);
			addPropertyString(result, typeId, filter, PropertyIds.VERSION_SERIES_CHECKED_OUT_BY,
					checkoutUser != null ? checkoutUser.getFullName() : null);
			addPropertyString(result, typeId, filter, PropertyIds.VERSION_SERIES_CHECKED_OUT_ID, getId(doc));
		}
		addPropertyString(result, typeId, filter, PropertyIds.CHECKIN_COMMENT, doc.getComment());
		addPropertyInteger(result, typeId, filter, PropertyIds.CONTENT_STREAM_LENGTH, doc.getFileSize());
		objectInfo.setHasContent(true);

		if (doc.getFileName() == null) {
			addPropertyString(result, typeId, filter, PropertyIds.CONTENT_STREAM_MIME_TYPE, null);
			addPropertyString(result, typeId, filter, PropertyIds.CONTENT_STREAM_FILE_NAME, null);

			objectInfo.setContentType(null);
			objectInfo.setFileName(null);
		} else {
			addPropertyString(result, typeId, filter, PropertyIds.CONTENT_STREAM_MIME_TYPE,
					MimeTypes.getMIMEType(FileUtil.getExtension(doc.getFileName())));
			addPropertyString(result, typeId, filter, PropertyIds.CONTENT_STREAM_FILE_NAME, doc.getFileName());

			objectInfo.setContentType(FileUtil.getExtension(doc.getFileName()));
			objectInfo.setFileName(doc.getFileName());
		}

		addPropertyId(result, typeId, filter, PropertyIds.CONTENT_STREAM_ID, null);
		addPropertyBoolean(result, typeId, filter, PropertyIds.IS_IMMUTABLE, doc.getImmutable() != 0);

		addPropertyString(result, typeId, filter, TypeManager.PROP_LANGUAGE, doc.getLanguage());
		addPropertyInteger(result, typeId, filter, TypeManager.PROP_RATING,
				doc.getRating() != null ? doc.getRating() : 0);
		addPropertyString(result, typeId, filter, TypeManager.PROP_FILEVERSION, doc.getFileVersion());
		addPropertyString(result, typeId, filter, TypeManager.PROP_VERSION, doc.getVersion());
		addPropertyString(result, typeId, filter, TypeManager.PROP_CUSTOMID, doc.getCustomId());

		try {
			addPropertyString(result, typeId, filter, TypeManager.PROP_TAGS, doc.getTgs());
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		compileDocumentOrVersionExtAttrsProperties(doc, typeId, result, filter);
	}

	private void compileDocumentOrVersionExtAttrsProperties(AbstractDocument doc, String typeId, PropertiesImpl result,
			Set<String> filter) throws PersistenceException {

		Template template = getTemplate(doc);

		if (template == null) {
			addPropertyString(result, typeId, filter, TypeManager.PROP_TEMPLATE, null);
			return;
		}

		/*
		 * Fill the extended attributes but only if we are not dealing with a
		 * search hit
		 */
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		addPropertyString(result, typeId, filter, TypeManager.PROP_TEMPLATE, template.getName());

		if (doc instanceof Document)
			documentDao.initialize((Document) doc);
		else
			versionDao.initialize((Version) doc);

		// Now load the extended properties
		Map<String, Attribute> attributes = doc.getAttributes();
		for (Entry<String, Attribute> entry : attributes.entrySet()) {
			String attrName = entry.getKey();
			Attribute attribute = entry.getValue();
			String stringValue = null;
			if (attribute.getValue() != null)
				switch (attribute.getType()) {
				case Attribute.TYPE_BOOLEAN:
					stringValue = Long.toString(attribute.getIntValue());
					break;
				case Attribute.TYPE_DATE:
					stringValue = df.format(attribute.getDateValue());
					break;
				case Attribute.TYPE_DOUBLE:
					stringValue = attribute.getDoubleValue() != null ? attribute.getDoubleValue().toString() : null;
					break;
				case Attribute.TYPE_INT:
					stringValue = Long.toString(attribute.getIntValue());
					break;
				case Attribute.TYPE_USER:
					stringValue = Long.toString(attribute.getIntValue());
					break;
				default:
					stringValue = attribute.getValue().toString();
				}
			addPropertyString(result, typeId, filter, TypeManager.PROP_EXT + attrName, stringValue);
		}
	}

	private Template getTemplate(AbstractDocument doc) throws PersistenceException {
		Template template = doc.getTemplate();
		if (doc instanceof Version && ((Version) doc).getTemplateName() != null)
			template = templateDao.findByName(((Version) doc).getTemplateName(), doc.getTenantId());
		return template;
	}

	private void compileFolderProperties(Folder folder, ObjectInfoImpl objectInfo, Set<String> filter, String typeId,
			PropertiesImpl result) throws PersistenceException {
		GregorianCalendar creation = millisToCalendar(folder.getCreation().getTime());

		// created and modified by
		addPropertyString(result, typeId, filter, PropertyIds.CREATED_BY, folder.getCreator());
		addPropertyString(result, typeId, filter, PropertyIds.LAST_MODIFIED_BY, USER_UNKNOWN);
		objectInfo.setCreatedBy(folder.getCreator());

		addPropertyDateTime(result, typeId, filter, PropertyIds.CREATION_DATE, creation);
		objectInfo.setCreationDate(creation);

		// base type and type name
		addPropertyId(result, typeId, filter, PropertyIds.BASE_TYPE_ID, BaseTypeId.CMIS_FOLDER.value());
		if (folder.getType() == 1) {
			addPropertyId(result, typeId, filter, PropertyIds.OBJECT_TYPE_ID, TypeManager.WORKSPACE_TYPE_ID);
		} else {
			addPropertyId(result, typeId, filter, PropertyIds.OBJECT_TYPE_ID, TypeManager.FOLDER_TYPE_ID);
		}

		String path = folderDao.computePathExtended(folder.getId());
		addPropertyString(result, typeId, filter, PropertyIds.PATH, (path.length() == 0 ? "/" : path));

		// folder properties
		if (!root.equals(folder)) {
			addPropertyId(result, typeId, filter, PropertyIds.PARENT_ID, ID_PREFIX_FLD + folder.getParentId());
			objectInfo.setHasParent(true);
		} else {
			addPropertyId(result, typeId, filter, PropertyIds.PARENT_ID, null);
			objectInfo.setHasParent(false);
		}
		addPropertyIdList(result, typeId, filter, PropertyIds.ALLOWED_CHILD_OBJECT_TYPE_IDS, null);
		addPropertyString(result, typeId, filter, TypeManager.PROP_DESCRIPTION, folder.getDescription());

		// Identifica il tipo della cartella: workspace o normale
		addPropertyInteger(result, typeId, filter, TypeManager.PROP_TYPE, folder.getType());
	}

	public List<ObjectData> getAllVersions(String objectId) {
		validatePermission(objectId, null, null);

		List<ObjectData> versions = new ArrayList<>();

		AbstractDocument doc = getDocument(objectId);

		List<Version> buf = versionDao.findByDocId(doc.getId());

		if (doc instanceof Document) {
			for (Version version : buf) {
				ObjectData data = compileObjectType(null, version, null, true, false, null);
				versions.add(data);
			}
		} else {
			versions.add(compileObjectType(null, doc, null, true, false, null));
		}

		return versions;
	}

	/**
	 * Alters the documents metadata by getting the informations from the
	 * properties
	 * 
	 * @param doc The document to update
	 * @param properties The properties to use
	 * @param create True if we are updating metadata for creating a new element
	 * 
	 * @throws PersistenceException Error in the data layer 
	 */
	private void updateDocumentMetadata(AbstractDocument doc, Properties properties, boolean create) throws PersistenceException {
		log.debug("updateDocumentMetadata doc: {}", doc);
		log.debug("updateDocumentMetadata properties: {}", properties);

		// get the property definitions
		TypeDefinition type = types.getType(TypeManager.DOCUMENT_TYPE_ID);

		// update properties
		for (PropertyData<?> p : properties.getProperties().values()) {
			updateDocumentMetadata(doc, properties, p, create, type);
		}
	}

	private void updateDocumentMetadata(AbstractDocument doc, Properties properties, PropertyData<?> p, boolean create,
			TypeDefinition type) throws PersistenceException {
		PropertyDefinition<?> propType = type.getPropertyDefinitions().get(p.getId());

		// do we know that property?
		if (propType == null)
			throw new CmisConstraintException(String.format(PROPERTY, p.getId(), IS_UNKNOWN));

		// can it be set?
		if ((propType.getUpdatability() == Updatability.READONLY))
			throw new CmisConstraintException(String.format(PROPERTY, p.getId(), "is readonly!"));

		if (propType.getUpdatability() == Updatability.ONCREATE && !create)
			throw new CmisConstraintException(String.format(PROPERTY, p.getId(), "can only be set on create!"));

		if ((p.getId().equals(PropertyIds.CONTENT_STREAM_FILE_NAME) || p.getId().equals(PropertyIds.NAME))) {
			updateDocumentFileName(doc, p);
		} else if (p.getId().equals(TypeManager.PROP_CUSTOMID)) {
			doc.setCustomId((String) p.getFirstValue());
		} else if (p.getId().equals(TypeManager.PROP_LANGUAGE)) {
			updateDocumentLocale(doc, p);
		} else if (p.getId().equals(TypeManager.PROP_TAGS)) {
			updateDocumentTags(doc, p);
		} else if (p.getId().equals(TypeManager.PROP_TEMPLATE)) {
			updateDocumentTemplate(doc, p);
		} else if (p.getId().startsWith(TypeManager.PROP_EXT)) {
			/*
			 * This is an extended attribute
			 */
			updateDocumentExtendedAttribute(doc, properties, p);
		}
	}

	private void updateDocumentFileName(AbstractDocument doc, PropertyData<?> p) {
		if (StringUtils.isNotEmpty((String) p.getFirstValue()))
			doc.setFileName((String) p.getFirstValue());
	}

	private void updateDocumentLocale(AbstractDocument doc, PropertyData<?> p) {
		LanguageManager langMan = LanguageManager.getInstance();
		Language lang = langMan.getLanguage(LocaleUtil.toLocale((String) p.getFirstValue()));
		if (lang != null)
			doc.setLocale(LocaleUtil.toLocale((String) p.getFirstValue()));
	}

	private void updateDocumentTags(AbstractDocument doc, PropertyData<?> p) {
		doc.getTags().clear();
		doc.setTgs((String) p.getFirstValue());
		if (doc.getTgs() != null) {
			StringTokenizer st = new StringTokenizer(doc.getTgs(), ",", false);
			while (st.hasMoreTokens()) {
				String tg = st.nextToken();
				if (StringUtils.isNotEmpty(tg))
					doc.addTag(tg);
			}
		}
	}

	private void updateDocumentTemplate(AbstractDocument doc, PropertyData<?> p) throws PersistenceException {
		if (p.getFirstValue() == null) {
			doc.setTemplate(null);
			if (doc instanceof Document)
				((Document) doc).setTemplate(null);
			else
				((Version) doc).setTemplateName(null);
		} else {
			Template template = templateDao.findByName((String) p.getFirstValue(), doc.getTenantId());
			doc.setTemplate(template);
			doc.setTemplateName(template != null ? template.getName() : null);
			doc.setTemplateId(template != null ? template.getId() : null);
		}
	}

	private void updateDocumentExtendedAttribute(AbstractDocument doc, Properties properties, PropertyData<?> p) throws PersistenceException {
		// try to load the document template first
		Template template = null;
		PropertyData<?> tp = properties.getProperties().get(TypeManager.PROP_TEMPLATE);
		if (tp != null)
			template = templateDao.findByName((String) tp.getFirstValue(), doc.getTenantId());

		if (template == null)
			return;

		templateDao.initialize(template);

		String attributeName = p.getId().substring(TypeManager.PROP_EXT.length());
		String stringValue = (String) p.getFirstValue();

		Attribute attribute = template.getAttribute(attributeName);

		switch (attribute.getType()) {
		case Attribute.TYPE_BOOLEAN:
			updateDocumentBooleanValue(doc, attributeName, stringValue);
			break;
		case Attribute.TYPE_DATE:
			updateDocumentDateValue(doc, attributeName, stringValue);
			break;
		case Attribute.TYPE_DOUBLE:
			updateDocumentDoubleValue(doc, attributeName, stringValue);
			break;
		case Attribute.TYPE_INT:
			updateDocumentIntValue(doc, attributeName, stringValue);
			break;
		case Attribute.TYPE_USER:
			updateDocumentUserValue(doc, attributeName, stringValue);
			break;
		case Attribute.TYPE_STRING:
			doc.setValue(attributeName, stringValue);
			break;
		default:
			// nothing to do here
			break;
		}
	}

	private void updateDocumentUserValue(AbstractDocument doc, String attributeName, String stringValue) {
		if (StringUtils.isNotEmpty(stringValue)) {
			try {
				doc.setValue(attributeName, userDao.findById(Long.parseLong(stringValue)));
			} catch (NumberFormatException | PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		} else
			doc.setValue(attributeName, (User) null);
	}

	private void updateDocumentIntValue(AbstractDocument doc, String attributeName, String stringValue) {
		if (StringUtils.isNotEmpty(stringValue))
			doc.setValue(attributeName, Long.parseLong(stringValue));
		else
			doc.setValue(attributeName, (Long) null);
	}

	private void updateDocumentDoubleValue(AbstractDocument doc, String attributeName, String stringValue) {
		if (StringUtils.isNotEmpty(stringValue))
			doc.setValue(attributeName, Double.parseDouble(stringValue));
		else
			doc.setValue(attributeName, (Double) null);
	}

	private void updateDocumentDateValue(AbstractDocument doc, String attributeName, String stringValue) {
		if (StringUtils.isNotEmpty(stringValue))
			try {
				DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
				doc.setValue(attributeName, df.parse(stringValue));
			} catch (ParseException e) {
				log.error("Invalid date {}", stringValue);
				doc.setValue(attributeName, (Date) null);
			}
		else
			doc.setValue(attributeName, (Date) null);
	}

	private void updateDocumentBooleanValue(AbstractDocument doc, String attributeName, String stringValue) {
		if (StringUtils.isNotEmpty(stringValue))
			doc.setValue(attributeName, Boolean.valueOf(("1".equals(stringValue) || "true".equals(stringValue))));
		else
			doc.setValue(attributeName, (Boolean) null);
	}

	/**
	 * Checks and updates a property set and write to database.
	 * 
	 * @throws PersistenceException Storage error during update
	 */
	private Properties update(PersistentObject object, Properties oldProperties, Properties properties)
			throws PersistenceException {
		PropertiesImpl result = new PropertiesImpl();

		if (properties == null)
			throw new CmisConstraintException("No properties!");

		String typeId = object instanceof Document ? TypeManager.DOCUMENT_TYPE_ID : TypeManager.FOLDER_TYPE_ID;

		// get the property definitions
		TypeDefinition type = types.getType(typeId);

		// copy old properties
		copyOldProperties(type, oldProperties, result);

		// now put the new properties, update properties
		copyProperties(type, properties, result);

		if (object instanceof Document) {
			Document doc = (Document) object;
			documentDao.initialize(doc);
			updateDocumentMetadata(doc, result, false);

			addPropertyId(result, typeId, null, PropertyIds.OBJECT_TYPE_ID, typeId);
			addPropertyString(result, typeId, null, PropertyIds.LAST_MODIFIED_BY, getSessionUser().getFullName());

			DocumentHistory transaction = new DocumentHistory();
			transaction.setUser(getSessionUser());
			transaction.setSessionId(sid);
			transaction.setEvent(DocumentEvent.CHANGED.toString());
			Document actualDoc = documentDao.findById(doc.getId());
			documentDao.initialize(actualDoc);
			doc.setId(0);
			documentManager.update(actualDoc, doc, transaction);
		} else {
			Folder folder = (Folder) object;
			updateFolderMetadata(folder, result, properties);

			addPropertyId(result, typeId, null, PropertyIds.OBJECT_TYPE_ID, typeId);
			addPropertyString(result, typeId, null, PropertyIds.LAST_MODIFIED_BY, getSessionUser().getFullName());

			FolderHistory transaction = new FolderHistory();
			transaction.setUser(getSessionUser());
			transaction.setSessionId(sid);

			// FolderEvent.RENAMED for the LDSynch
			transaction.setEvent(FolderEvent.CHANGED.toString());
			folderDao.store(folder, transaction);
		}

		return result;
	}

	private void copyProperties(TypeDefinition type, Properties properties, PropertiesImpl result) {
		for (PropertyData<?> prop : properties.getProperties().values()) {
			PropertyDefinition<?> propType = type.getPropertyDefinitions().get(prop.getId());

			// do we know that property?
			if (propType == null)
				throw new CmisConstraintException(String.format(PROPERTY, prop.getId(), IS_UNKNOWN));

			// can it be set?
			if ((propType.getUpdatability() == Updatability.READONLY))
				throw new CmisConstraintException(String.format(PROPERTY, prop.getId(), "is readonly!"));

			if ((propType.getUpdatability() == Updatability.ONCREATE))
				throw new CmisConstraintException(String.format(PROPERTY, prop.getId(), "can only be set on create!"));

			// default or value
			if (isEmptyProperty(prop)) {
				addPropertyDefault(result, propType);
			} else {
				result.addProperty(prop);
			}
		}
	}

	private void copyOldProperties(TypeDefinition type, Properties oldProperties, PropertiesImpl newProperties) {
		for (PropertyData<?> prop : oldProperties.getProperties().values()) {
			PropertyDefinition<?> propType = type.getPropertyDefinitions().get(prop.getId());

			// do we know that property?
			if (propType == null)
				throw new CmisConstraintException(String.format(PROPERTY, prop.getId(), IS_UNKNOWN));

			// only add read/write properties
			if ((propType.getUpdatability() != Updatability.READWRITE))
				continue;

			newProperties.addProperty(prop);
		}
	}

	private void updateFolderMetadata(Folder folder, PropertiesImpl result, Properties properties) {
		// update properties
		for (PropertyData<?> prop : properties.getProperties().values()) {
			PropertyData<?> p = result.getProperties().get(prop.getId());

			if ((p.getId().equals(PropertyIds.CONTENT_STREAM_FILE_NAME) || p.getId().equals(PropertyIds.NAME))
					&& StringUtils.isNotEmpty((String) p.getFirstValue()))
				folder.setName((String) p.getFirstValue());
			else if (p.getId().equals(TypeManager.PROP_DESCRIPTION)) {
				folder.setDescription((String) p.getFirstValue());
			}
		}
	}

	private static boolean isEmptyProperty(PropertyData<?> prop) {
		if ((prop == null) || (prop.getValues() == null)) {
			return true;
		}

		return prop.getValues().isEmpty();
	}

	private void addPropertyId(PropertiesImpl props, String typeId, Set<String> filter, String id, String value) {
		if (!checkAddProperty(props, typeId, filter, id)) {
			return;
		}

		PropertyIdImpl p = new PropertyIdImpl(id, value);
		p.setQueryName(id);
		props.addProperty(p);
	}

	private void addPropertyIdList(PropertiesImpl props, String typeId, Set<String> filter, String id,
			List<String> value) {
		if (!checkAddProperty(props, typeId, filter, id)) {
			return;
		}

		PropertyIdImpl p = new PropertyIdImpl(id, value);
		p.setQueryName(id);
		props.addProperty(p);
	}

	private void addPropertyString(PropertiesImpl props, String typeId, Set<String> filter, String id, String value) {
		if (!checkAddProperty(props, typeId, filter, id)) {
			return;
		}
		PropertyStringImpl p = new PropertyStringImpl(id, value);
		p.setQueryName(id);
		props.addProperty(p);
	}

	private void addPropertyInteger(PropertiesImpl props, String typeId, Set<String> filter, String id, long value) {
		addPropertyBigInteger(props, typeId, filter, id, BigInteger.valueOf(value));
	}

	private void addPropertyBigInteger(PropertiesImpl props, String typeId, Set<String> filter, String id,
			BigInteger value) {
		if (!checkAddProperty(props, typeId, filter, id)) {
			return;
		}

		PropertyIntegerImpl p = new PropertyIntegerImpl(id, value);
		p.setQueryName(id);
		props.addProperty(p);
	}

	private void addPropertyBoolean(PropertiesImpl props, String typeId, Set<String> filter, String id, boolean value) {
		if (!checkAddProperty(props, typeId, filter, id)) {
			return;
		}

		PropertyBooleanImpl p = new PropertyBooleanImpl(id, value);
		p.setQueryName(id);
		props.addProperty(p);
	}

	private void addPropertyDateTime(PropertiesImpl props, String typeId, Set<String> filter, String id,
			GregorianCalendar value) {

		if (!checkAddProperty(props, typeId, filter, id)) {
			return;
		}

		PropertyDateTimeImpl p = new PropertyDateTimeImpl(id, value);
		p.setQueryName(id);
		props.addProperty(p);
	}

	private boolean checkAddProperty(Properties properties, String typeId, Set<String> filter, String id) {
		try {
			if ((properties == null) || (properties.getProperties() == null)) {
				throw new IllegalArgumentException("Properties must not be null!");
			}

			if (id == null) {
				throw new IllegalArgumentException("Id must not be null!");
			}

			TypeDefinition type = types.getType(typeId);
			if (type == null) {
				throw new IllegalArgumentException("Unknown type: " + typeId);
			}

			if (!type.getPropertyDefinitions().containsKey(id)) {
				throw new IllegalArgumentException("Unknown property: " + id);
			}

			String queryName = type.getPropertyDefinitions().get(id).getQueryName();

			if ((queryName != null) && (filter != null)) {
				if (!filter.contains(queryName)) {
					return false;
				} else {
					filter.remove(queryName);
				}
			}

			return true;
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}

		return false;
	}

	/**
	 * Adds the default value of property if defined.
	 */
	@SuppressWarnings("unchecked")
	private static boolean addPropertyDefault(PropertiesImpl props, PropertyDefinition<?> propDef) {
		if ((props == null) || (props.getProperties() == null)) {
			throw new IllegalArgumentException("Props must not be null!");
		}

		if (propDef == null) {
			return false;
		}

		List<?> defaultValue = propDef.getDefaultValue();
		if ((defaultValue != null) && (!defaultValue.isEmpty())) {
			switch (propDef.getPropertyType()) {
			case BOOLEAN:
				PropertyBooleanImpl p = new PropertyBooleanImpl(propDef.getId(), (List<Boolean>) defaultValue);
				p.setQueryName(propDef.getId());
				props.addProperty(p);
				break;
			case DATETIME:
				PropertyDateTimeImpl p1 = new PropertyDateTimeImpl(propDef.getId(),
						(List<GregorianCalendar>) defaultValue);
				p1.setQueryName(propDef.getId());
				props.addProperty(p1);
				break;
			case DECIMAL:
				PropertyDecimalImpl p3 = new PropertyDecimalImpl(propDef.getId(), (List<BigDecimal>) defaultValue);
				p3.setQueryName(propDef.getId());
				props.addProperty(p3);
				break;
			case HTML:
				PropertyHtmlImpl p4 = new PropertyHtmlImpl(propDef.getId(), (List<String>) defaultValue);
				p4.setQueryName(propDef.getId());
				props.addProperty(p4);
				break;
			case ID:
				PropertyIdImpl p5 = new PropertyIdImpl(propDef.getId(), (List<String>) defaultValue);
				p5.setQueryName(propDef.getId());
				props.addProperty(p5);
				break;
			case INTEGER:
				PropertyIntegerImpl p6 = new PropertyIntegerImpl(propDef.getId(), (List<BigInteger>) defaultValue);
				p6.setQueryName(propDef.getId());
				props.addProperty(p6);
				break;
			case STRING:
				PropertyStringImpl p7 = new PropertyStringImpl(propDef.getId(), (List<String>) defaultValue);
				p7.setQueryName(propDef.getId());
				props.addProperty(p7);
				break;
			case URI:
				PropertyUriImpl p8 = new PropertyUriImpl(propDef.getId(), (List<String>) defaultValue);
				p8.setQueryName(propDef.getId());
				props.addProperty(p8);
				break;
			default:
				throw new IllegalArgumentException("Unknown datatype! Spec change?");
			}

			return true;
		}

		return false;
	}

	/**
	 * Compiles the allowable actions for a file or folder.
	 */
	private AllowableActions compileAllowableActions(PersistentObject object) {
		if (object == null) {
			throw new IllegalArgumentException("Object must not be null!");
		}

		boolean write = checkPermission(object, null, Permission.WRITE);
		boolean download = checkPermission(object, null, Permission.DOWNLOAD);

		boolean isFolder = object instanceof Folder;
		boolean isWorkspace = isFolder && ((Folder) object).getType() == Folder.TYPE_WORKSPACE;
		boolean isRoot = root.equals(object);

		Set<Action> aas = new HashSet<>();

		addAction(aas, Action.CAN_GET_OBJECT_PARENTS, !isRoot);
		addAction(aas, Action.CAN_GET_PROPERTIES, true);
		addAction(aas, Action.CAN_GET_ACL, true);

		if (isFolder) {
			addAction(aas, Action.CAN_GET_DESCENDANTS, true);
			addAction(aas, Action.CAN_GET_CHILDREN, true);
			addAction(aas, Action.CAN_GET_FOLDER_PARENT, !isRoot);
			addAction(aas, Action.CAN_GET_FOLDER_TREE, true);
			addAction(aas, Action.CAN_CREATE_DOCUMENT, write);
			addAction(aas, Action.CAN_CREATE_FOLDER, write);
			addAction(aas, Action.CAN_ADD_OBJECT_TO_FOLDER, write);

			addAction(aas, Action.CAN_UPDATE_PROPERTIES, write && !isWorkspace);
			addAction(aas, Action.CAN_MOVE_OBJECT, write && download && !isWorkspace);
			addAction(aas, Action.CAN_DELETE_OBJECT, write && !isWorkspace);
			addAction(aas, Action.CAN_DELETE_TREE, write && !isWorkspace);
		} else if (object instanceof Document) {
			Document doc = (Document) object;

			addAction(aas, Action.CAN_UPDATE_PROPERTIES, write);
			addAction(aas, Action.CAN_MOVE_OBJECT, write && download);
			addAction(aas, Action.CAN_DELETE_OBJECT, write);
			addAction(aas, Action.CAN_GET_CONTENT_STREAM, true);
			addAction(aas, Action.CAN_GET_ALL_VERSIONS, true);
			addAction(aas, Action.CAN_SET_CONTENT_STREAM, write);
			addAction(aas, Action.CAN_CHECK_OUT, doc.getStatus() == AbstractDocument.DOC_UNLOCKED && write);
			addAction(aas, Action.CAN_CHECK_IN, doc.getStatus() == AbstractDocument.DOC_CHECKED_OUT
					&& doc.getLockUserId().longValue() == getSessionUser().getId() && write);
			addAction(aas, Action.CAN_CANCEL_CHECK_OUT,
					doc.getStatus() != AbstractDocument.DOC_UNLOCKED
							&& (doc.getLockUserId().longValue() == getSessionUser().getId()
									|| getSessionUser().isMemberOf(Group.GROUP_ADMIN)));
		} else {
			addAction(aas, Action.CAN_GET_CONTENT_STREAM, true);
		}

		AllowableActionsImpl result = new AllowableActionsImpl();
		result.setAllowableActions(aas);

		return result;
	}

	private static void addAction(Set<Action> aas, Action action, boolean condition) {
		if (condition) {
			aas.add(action);
		}
	}

	/**
	 * Compiles the ACL for a file or folder
	 * 
	 * @param object the persistent object
	 * 
	 * @return the ACL
	 */
	private Acl compileAcl(PersistentObject object) {
		AccessControlListImpl result = new AccessControlListImpl();
		result.setAces(new ArrayList<>());

		for (Map.Entry<String, Boolean> ue : userMap.entrySet()) {

			// create principal
			AccessControlPrincipalDataImpl principal = new AccessControlPrincipalDataImpl(ue.getKey());

			// create ACE
			AccessControlEntryImpl entry = new AccessControlEntryImpl();
			entry.setPrincipal(principal);
			entry.setPermissions(new ArrayList<>());
			entry.getPermissions().add(CMIS_READ);

			if (!ue.getValue().booleanValue() && checkPermission(object, null, Permission.WRITE)
					&& !(object instanceof Folder && ((Folder) object).getType() == Folder.TYPE_WORKSPACE)) {

				entry.getPermissions().add(CMIS_WRITE);
				entry.getPermissions().add(CMIS_ALL);
			}

			entry.setDirect(true);

			// add ACE
			result.getAces().add(entry);
		}

		return result;
	}

	/**
	 * Converts milliseconds into a calendar object.
	 */
	private static GregorianCalendar millisToCalendar(long millis) {
		GregorianCalendar result = new GregorianCalendar();
		result.setTimeZone(TimeZone.getTimeZone("GMT"));
		result.setTimeInMillis((long) (Math.ceil(millis / 1000D) * 1000D));

		return result;
	}

	/**
	 * Splits a filter statement into a collection of properties. If
	 * <code>filter</code> is <code>null</code>, empty or one of the properties
	 * is '*' a null is returned. Otherwise it will return a collection of
	 * filters.
	 */
	private static Set<String> splitFilter(String filter) {

		/**
		 * NOTE: pay very attention modifying this method, returning an empty
		 * HashSet<String>() instead of a null invalidates completely the
		 * working of the cmis module - Alex 2023-01-26
		 */
		if (filter == null) {
			return null;
		}

		if (filter.trim().length() == 0) {
			return null;
		}

		Set<String> result = new HashSet<>();
		for (String s : filter.split(",")) {
			s = s.trim();
			if (s.equals("*")) {
				return null;
			} else if (s.length() > 0) {
				result.add(s);
			}
		}

		// set a few base properties
		// query name == id (for base type properties)
		result.add(PropertyIds.OBJECT_ID);
		result.add(PropertyIds.OBJECT_TYPE_ID);
		result.add(PropertyIds.BASE_TYPE_ID);

		return result;
	}

	/**
	 * Gets the type id from a set of properties.
	 */
	private static String getTypeId(Properties properties) {
		PropertyData<?> typeProperty = properties.getProperties().get(PropertyIds.OBJECT_TYPE_ID);
		if (!(typeProperty instanceof PropertyId)) {
			throw new CmisInvalidArgumentException("Type id must be set!");
		}

		String typeId = ((PropertyId) typeProperty).getFirstValue();
		if (typeId == null) {
			throw new CmisInvalidArgumentException("Type id must be set!");
		}

		return typeId;
	}

	/**
	 * Returns the first value of an string property.
	 */
	static String getStringProperty(Properties properties, String name) {

		PropertyData<?> property = properties.getProperties().get(name);

		if (property == null) {
			return null;
		}
		if (property instanceof PropertyId) {
			return ((PropertyId) property).getFirstValue();
		}
		if (!(property instanceof PropertyString)) {
			return null;
		}

		return ((PropertyString) property).getFirstValue();
	}

	/**
	 * Gets the user associated to the current session.
	 */
	private User getSessionUser() {
		if (sid != null) {
			if (SessionManager.get().getStatus(sid) != Session.STATUS_OPEN)
				throw new CmisPermissionDeniedException("Invalid session");
			try {
				return userDao.findById(SessionManager.get().get(sid).getUserId());
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
				throw new CmisPermissionDeniedException("User not found");
			}
		} else {
			throw new CmisPermissionDeniedException("No session");
		}
	}

	private boolean checkPermission(PersistentObject object, long userId, Permission permission)
			throws PersistenceException {
		long objectId = root.getId();

		boolean enabled = false;
		if (object != null) {
			if (object instanceof Folder) {
				objectId = object.getId();
			} else if (object instanceof Version) {
				objectId = ((Version) object).getDocId();
				Document doc = documentDao.findById(objectId);
				objectId = doc.getFolder().getId();
			} else {
				objectId = ((Document) object).getFolder().getId();
			}
		}

		enabled = folderDao.isReadEnabled(objectId, userId);

		if (enabled && permission != null) {
			if (object instanceof Folder && objectId == Folder.ROOTID) {
				// The root is just readable
				enabled = permission.equals(Permission.READ);
			} else {
				enabled = folderDao.isPermissionEnabled(permission, objectId, userId);
			}
		}

		return enabled;
	}

	private boolean checkPermission(PersistentObject object, CallContext context, Permission permission) {
		long userId = 0;

		try {
			if (sid != null) {
				if (SessionManager.get().getStatus(sid) != Session.STATUS_OPEN)
					return false;
				else
					SessionManager.get().renew(sid);
				userId = SessionManager.get().get(sid).getUserId();
			} else if (context != null) {
				User user = userDao.findByUsername(context.getUsername());
				userId = user.getId();
			}

			return checkPermission(object, userId, permission);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return false;
		}

	}

	private void validatePermission(String objectId, CallContext context, Permission permission)
			throws CmisPermissionDeniedException {
		if (!checkPermission(objectId != null ? getObject(objectId) : null, context, permission)) {
			CmisPermissionDeniedException exception = new CmisPermissionDeniedException(
					"Permission " + (permission != null ? permission.getName() : "") + " not granted on " + objectId);
			if (log.isDebugEnabled())
				log.error(exception.getMessage(), exception);
			throw exception;
		}
	}

	private void debug(String msg) {
		log.debug("<{}> {}", id, msg);
	}

	public String getId() {
		return id;
	}

	/**
	 * Calculates the ObjectID for a persistent object. For documents it is
	 * doc.<b>docId</b>.
	 */
	private String getId(PersistentObject object) {
		if (object == null) {
			throw new IllegalArgumentException("Object is not valid!");
		}

		if (object instanceof Document)
			return ID_PREFIX_DOC + object.getId();
		else if (object instanceof Version)
			return ID_PREFIX_VER + object.getId();
		else
			return ID_PREFIX_FLD + object.getId();
	}

	/**
	 * Retrieves the instance by the given objectId
	 */
	private PersistentObject getObject(String objectId) {
		PersistentObject out = null;
		try {
			if (objectId.startsWith(ID_PREFIX_DOC)) {
				Long docId = Long.parseLong(objectId.substring(4));
				Document doc = documentDao.findDocument(docId);
				documentDao.initialize(doc);
				out = doc;
			} else if (objectId.startsWith(ID_PREFIX_FLD)) {
				Long folderId = Long.parseLong(objectId.substring(4));
				Folder f = folderDao.findFolder(folderId);
				folderDao.initialize(f);
				out = f;
			} else if (objectId.startsWith(ID_PREFIX_VER)) {
				Long versionId = Long.parseLong(objectId.substring(4));
				Version v = versionDao.findById(versionId);
				versionDao.initialize(v);
				out = v;
			} else {
				Long folderId = Long.parseLong(objectId);
				Folder f = folderDao.findFolder(folderId);
				folderDao.initialize(f);
				out = f;
			}
		} catch (NumberFormatException | PersistenceException e) {
			log.warn(e.getMessage(), e);
		}

		if (out == null)
			throw new CmisObjectNotFoundException(String.format("Object %s not found!", objectId));
		return out;
	}

	public void setTemplateDao(TemplateDAO templateDao) {
		this.templateDao = templateDao;
	}

	public ObjectList getContentChanges(Holder<String> changeLogToken, int max) throws CmisPermissionDeniedException {

		log.debug("getContentChanges {}", changeLogToken);

		if (changeLogToken == null)
			throw new CmisInvalidArgumentException("Missing change log token holder");

		long minDate;

		try {
			minDate = Long.parseLong(changeLogToken.getValue());
		} catch (NumberFormatException e) {
			throw new CmisInvalidArgumentException("Invalid change log token");
		}

		ObjectListImpl ol = new ObjectListImpl();

		List<ObjectData> odsDocs = getDocumentLastChanges(minDate, max);
		List<ObjectData> odsFolders = getFolderLastChanges(minDate, max);

		// put together the 2 lists
		List<ObjectData> complex = new ArrayList<>();
		complex.addAll(odsDocs);
		complex.addAll(odsFolders);

		// sort the content of list complex by date
		Collections.sort(complex, (o1, o2) -> o1.getChangeEventInfo().getChangeTime().getTime()
				.compareTo(o2.getChangeEventInfo().getChangeTime().getTime()));

		boolean hasMoreItems = complex.size() > max;
		if (hasMoreItems)
			complex = complex.subList(0, max);

		ol.setObjects(complex);

		Date date = null;
		if (!complex.isEmpty()) {
			ol.setNumItems(BigInteger.valueOf(-1));
			ol.setHasMoreItems(Boolean.valueOf(hasMoreItems));
			date = complex.get(complex.size() - 1).getChangeEventInfo().getChangeTime().getTime();
		} else {
			ol.setHasMoreItems(Boolean.valueOf(false));
			ol.setNumItems(BigInteger.ZERO);
		}

		String latestChangeLogToken = date == null ? null : String.valueOf(date.getTime());
		log.debug("latestChangeLogToken {}", latestChangeLogToken);
		changeLogToken.setValue(latestChangeLogToken);

		return ol;
	}

	public List<ObjectData> getDocumentLastChanges(long minDate, int max) {
		StringBuilder query = new StringBuilder(" " + PersistentObjectDAO.ENTITY + ".tenantId = :tenantId and "
				+ PersistentObjectDAO.ENTITY + ".date >= :minDate ");
		query.append(" and " + PersistentObjectDAO.ENTITY + ".event in ('");
		query.append(DocumentEvent.STORED);
		query.append("','");
		query.append(DocumentEvent.CHECKEDIN);
		query.append("','");
		query.append(DocumentEvent.MOVED);
		query.append("','");
		query.append(DocumentEvent.RENAMED);
		query.append("','");
		query.append(DocumentEvent.DELETED);
		query.append("')");

		List<DocumentHistory> entries = new ArrayList<>();

		try {
			Map<String, Object> params = new HashMap<>();
			params.put("tenantId", getRoot().getTenantId());
			params.put("minDate", new Date(minDate));

			entries = historyDao.findByWhere(query.toString(), params,
					"order by " + PersistentObjectDAO.ENTITY + ".date", max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		List<ObjectData> ods = new ArrayList<>(entries.size());
		Date date = null;
		for (DocumentHistory logEntry : entries) {
			ObjectDataImpl od = new ObjectDataImpl();
			ChangeEventInfoDataImpl cei = new ChangeEventInfoDataImpl();

			// change type
			String eventId = logEntry.getEvent();
			ChangeType changeType;
			if (DocumentEvent.STORED.toString().equals(eventId)) {
				changeType = ChangeType.CREATED;
			} else if (DocumentEvent.CHECKEDIN.toString().equals(eventId)
					|| DocumentEvent.RENAMED.toString().equals(eventId)
					|| DocumentEvent.MOVED.toString().equals(eventId)) {
				changeType = ChangeType.UPDATED;
			} else if (DocumentEvent.DELETED.toString().equals(eventId)) {
				changeType = ChangeType.DELETED;
			} else {
				continue;
			}
			cei.setChangeType(changeType);

			// change time
			GregorianCalendar changeTime = (GregorianCalendar) Calendar.getInstance();
			date = logEntry.getDate();
			changeTime.setTime(date);
			cei.setChangeTime(changeTime);
			od.setChangeEventInfo(cei);

			// properties: id, doc type
			PropertiesImpl properties = new PropertiesImpl();
			properties.addProperty(new PropertyIdImpl(PropertyIds.OBJECT_ID, ID_PREFIX_DOC + logEntry.getDocId()));
			properties.addProperty(new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID, BaseTypeId.CMIS_DOCUMENT.value()));
			od.setProperties(properties);
			ods.add(od);
		}
		return ods;
	}

	public List<ObjectData> getFolderLastChanges(long minDate, int max) {

		StringBuilder query = new StringBuilder(" " + PersistentObjectDAO.ENTITY + ".tenantId = :tenantId and "
				+ PersistentObjectDAO.ENTITY + ".date >= :minDate ");
		query.append(" and " + PersistentObjectDAO.ENTITY + ".event in ('");
		query.append(FolderEvent.CREATED);
		query.append("','");
		query.append(FolderEvent.RENAMED);
		query.append("','");
		query.append(FolderEvent.MOVED);
		query.append("','");
		query.append(FolderEvent.DELETED);
		query.append("')");

		List<FolderHistory> entries = new ArrayList<>();

		try {
			Map<String, Object> params = new HashMap<>();
			params.put("tenantId", getRoot().getTenantId());
			params.put("minDate", new Date(minDate));
			entries = folderHistoryDao.findByWhere(query.toString(), params,
					"order by " + PersistentObjectDAO.ENTITY + ".date", max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		List<ObjectData> ods = new ArrayList<>(entries.size());
		Date date = null;
		for (FolderHistory logEntry : entries) {
			ObjectDataImpl od = new ObjectDataImpl();
			ChangeEventInfoDataImpl cei = new ChangeEventInfoDataImpl();

			// change type
			String eventId = logEntry.getEvent();
			ChangeType changeType;
			if (FolderEvent.CREATED.toString().equals(eventId)) {
				changeType = ChangeType.CREATED;
			} else if (FolderEvent.RENAMED.toString().equals(eventId) || FolderEvent.MOVED.toString().equals(eventId)) {
				changeType = ChangeType.UPDATED;
			} else if (FolderEvent.DELETED.toString().equals(eventId)) {
				changeType = ChangeType.DELETED;
			} else {
				continue;
			}
			cei.setChangeType(changeType);

			// change time
			GregorianCalendar changeTime = (GregorianCalendar) Calendar.getInstance();
			date = logEntry.getDate();
			changeTime.setTime(date);
			cei.setChangeTime(changeTime);
			od.setChangeEventInfo(cei);

			// properties: id, doc type
			PropertiesImpl properties = new PropertiesImpl();
			properties.addProperty(new PropertyIdImpl(PropertyIds.OBJECT_ID, ID_PREFIX_FLD + logEntry.getFolderId()));
			properties.addProperty(new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID, BaseTypeId.CMIS_FOLDER.value()));
			od.setProperties(properties);
			ods.add(od);
		}
		return ods;
	}

	public Folder getRoot() {
		return root;
	}

	public void setHistoryDao(DocumentHistoryDAO historyDao) {
		this.historyDao = historyDao;
	}
}