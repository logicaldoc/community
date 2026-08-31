package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * An Import Folder representation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIImportFolder implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final String PROVIDER_SMB = "smb";

	public static final String PROVIDER_SMB2 = "smb2";

	public static final String PROVIDER_SMB3 = "smb3";

	public static final String PROVIDER_FILE = "file";

	public static final String PROVIDER_FTP = "ftp";

	public static final String PROVIDER_FTPS = "ftps";

	public static final String PROVIDER_SFTP = "sftp";

	private long id = 0;

	private String path;

	private String domain;

	private String username;

	private String password;

	private String provider = "smb";

	private GUIFolder target = null;

	private int enabled = 1;

	private Integer maxSize = null;

	private int extractTags = 0;

	private String includes = "*.*";

	private String excludes = "~*,*~,#*#,.#*,%*%,._*,.DS_Store,desktop.ini,Thumbs";

	private String tags = "";

	private String language = "";

	private Long templateId;

	private Long ocrTemplateId;

	private Long barcodeTemplateId;

	private int depth = 5;

	private boolean delImport = false;

	private boolean importEmpty = false;

	private Date startDate;

	private int updatePolicy = 0;

	private boolean inheritRights = true;

	private boolean preventDuplications = false;

	private boolean recordHistory = false;

	private Integer port = null;

	private String host;

	private Long batch = 10000L;

	private String automation;

	private String automationAfter;
	
	private String automationEnd;

	public GUIImportFolder() {
		super();
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getProvider() {
		return provider;
	}

	public void setProvider(String provider) {
		this.provider = provider;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public Integer getMaxSize() {
		return maxSize;
	}

	public void setMaxSize(Integer maxSize) {
		this.maxSize = maxSize;
	}

	public int getExtractTags() {
		return extractTags;
	}

	public void setExtractTags(int extractTags) {
		this.extractTags = extractTags;
	}

	public String getIncludes() {
		return includes;
	}

	public void setIncludes(String includes) {
		this.includes = includes;
	}

	public String getExcludes() {
		return excludes;
	}

	public void setExcludes(String excludes) {
		this.excludes = excludes;
	}

	public String getTags() {
		return tags;
	}

	public void setTags(String tags) {
		this.tags = tags;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public int getDepth() {
		return depth;
	}

	public void setDepth(int depth) {
		this.depth = depth;
	}

	public boolean isDelImport() {
		return delImport;
	}

	public void setDelImport(boolean delImport) {
		this.delImport = delImport;
	}

	public GUIFolder getTarget() {
		return target;
	}

	public void setTarget(GUIFolder target) {
		this.target = target;
	}

	public String getDomain() {
		return domain;
	}

	public void setDomain(String domain) {
		this.domain = domain;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public int getUpdatePolicy() {
		return updatePolicy;
	}

	public void setUpdatePolicy(int updatePolicy) {
		this.updatePolicy = updatePolicy;
	}

	public boolean isImportEmpty() {
		return importEmpty;
	}

	public void setImportEmpty(boolean importEmpty) {
		this.importEmpty = importEmpty;
	}

	public boolean isInheritRights() {
		return inheritRights;
	}

	public void setInheritRights(boolean inheritRights) {
		this.inheritRights = inheritRights;
	}

	public Integer getPort() {
		return port;
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public Long getBatch() {
		return batch;
	}

	public void setBatch(Long batch) {
		this.batch = batch;
	}

	/**
	 * Composes the displayable importFolder's url.
	 * 
	 * @return the URL to display
	 */
	public String getDisplayUrl() {
		String url = "";
		if (PROVIDER_FILE.equals(getProvider())) {
			if (getPath() != null && !getPath().isEmpty())
				url += getPath();
		} else if (getProvider().startsWith(PROVIDER_SMB)) {
			String importFolderPath = getPath().replace("/", "\\\\");
			if (!importFolderPath.startsWith("\\"))
				importFolderPath = "\\" + importFolderPath;
			url += importFolderPath;
		} else {
			url += getProvider();
			url += "://";
			url += getHost();
			if (getPort() != null && getPort() > 0) {
				url += ":";
				url += getPort();
			}
			if (!getPath().startsWith("/"))
				url += "/";
			url += getPath();
		}
		return url;
	}

	public Long getOcrTemplateId() {
		return ocrTemplateId;
	}

	public void setOcrTemplateId(Long ocrTemplateId) {
		this.ocrTemplateId = ocrTemplateId;
	}

	public Long getBarcodeTemplateId() {
		return barcodeTemplateId;
	}

	public void setBarcodeTemplateId(Long barcodeTemplateId) {
		this.barcodeTemplateId = barcodeTemplateId;
	}

	public boolean isPreventDuplications() {
		return preventDuplications;
	}

	public void setPreventDuplications(boolean preventDuplications) {
		this.preventDuplications = preventDuplications;
	}

	public boolean isRecordHistory() {
		return recordHistory;
	}

	public void setRecordHistory(boolean recordHistory) {
		this.recordHistory = recordHistory;
	}

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}

	public String getAutomationAfter() {
		return automationAfter;
	}

	public void setAutomationAfter(String automationAfter) {
		this.automationAfter = automationAfter;
	}

	public String getAutomationEnd() {
		return automationEnd;
	}

	public void setAutomationEnd(String automationEnd) {
		this.automationEnd = automationEnd;
	}
}