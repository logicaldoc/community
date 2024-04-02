package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Bean to carry the security access settings for a given entity(user or group)
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIAccessControlEntry implements Serializable {

	private static final long serialVersionUID = 1L;

	private long entityId = 0;

	private String name;

	private String label;

	/*
	 * A set of permissions, cannot use a map because of they are not supported
	 * by default GWT serialization
	 */
	private ArrayList<GUIValue> permissions = new ArrayList<>();

	public static final String PERMISSION_DELETE = "delete";

	public static final String PERMISSION_IMMUTABLE = "immutable";

	public static final String PERMISSION_SECURITY = "security";

	public static final String PERMISSION_READ = "read";

	public static final String PERMISSION_PREVIEW = "preview";

	public static final String PERMISSION_WRITE = "write";

	public static final String PERMISSION_ADD = "add";

	public static final String PERMISSION_RENAME = "rename";

	public static final String PERMISSION_IMPORT = "import";

	public static final String PERMISSION_EXPORT = "export";

	public static final String PERMISSION_SIGN = "sign";

	public static final String PERMISSION_ARCHIVE = "archive";

	public static final String PERMISSION_WORKFLOW = "workflow";

	public static final String PERMISSION_DOWNLOAD = "download";

	public static final String PERMISSION_CALENDAR = "calendar";

	public static final String PERMISSION_SUBSCRIPTION = "subscription";

	public static final String PERMISSION_PASSWORD = "password";

	public static final String PERMISSION_MOVE = "move";

	public static final String PERMISSION_EMAIL = "email";

	public static final String PERMISSION_AUTOMATION = "automation";

	public static final String PERMISSION_STORAGE = "storage";

	public static final String PERMISSION_READINGREQ = "readingreq";

	public static final String PERMISSION_PRINT = "print";

	public GUIAccessControlEntry() {
	}

	public GUIAccessControlEntry(String... allowedPermissions) {
		permissions.clear();
		for (String permission : allowedPermissions) {
			permissions.add(new GUIValue(permission.toLowerCase(), "true"));
		}
	}

	public long getEntityId() {
		return entityId;
	}

	public void setEntityId(long entityId) {
		this.entityId = entityId;
	}

	public boolean isRead() {
		return isPermissionAllowed(PERMISSION_READ);
	}

	public boolean isPreview() {
		return isPermissionAllowed(PERMISSION_PREVIEW);
	}

	private GUIValue getPermission(String permission) {
		for (GUIValue guiValue : permissions)
			if (guiValue.getCode().equalsIgnoreCase(permission))
				return guiValue;
		return null;
	}

	private void setPermissionValue(String permission, boolean value) {
		GUIValue val = getPermission(permission);
		if (val == null) {
			val = new GUIValue(permission, Boolean.toString(value));
			permissions.add(val);
		} else {
			val.setValue(Boolean.toString(value));
		}
	}

	public void setRead(boolean read) {
		setPermissionValue(PERMISSION_READ, read);
	}

	public void setPreview(boolean preview) {
		setPermissionValue(PERMISSION_PREVIEW, preview);
	}

	public boolean isWrite() {
		return isPermissionAllowed(PERMISSION_WRITE);
	}

	public void setWrite(boolean write) {
		setPermissionValue(PERMISSION_WRITE, write);
	}

	public boolean isDelete() {
		return isPermissionAllowed(PERMISSION_DELETE);
	}

	public void setDelete(boolean delete) {
		setPermissionValue(PERMISSION_DELETE, delete);
	}

	public boolean isAdd() {
		return isPermissionAllowed(PERMISSION_ADD);
	}

	public void setAdd(boolean add) {
		setPermissionValue(PERMISSION_ADD, add);
	}

	public boolean isImport() {
		return isPermissionAllowed(PERMISSION_IMPORT);
	}

	public void setImport(boolean iimport) {
		setPermissionValue(PERMISSION_IMPORT, iimport);
	}

	public boolean isWorkflow() {
		return isPermissionAllowed(PERMISSION_WORKFLOW);
	}

	public void setWorkflow(boolean workflow) {
		setPermissionValue(PERMISSION_WORKFLOW, workflow);
	}

	public boolean isSign() {
		return isPermissionAllowed(PERMISSION_SIGN);
	}

	public void setSign(boolean sign) {
		setPermissionValue(PERMISSION_SIGN, sign);
	}

	public boolean isExport() {
		return isPermissionAllowed(PERMISSION_EXPORT);
	}

	public void setExport(boolean export) {
		setPermissionValue(PERMISSION_EXPORT, export);
	}

	public boolean isImmutable() {
		return isPermissionAllowed(PERMISSION_IMMUTABLE);
	}

	public void setImmutable(boolean immutable) {
		setPermissionValue(PERMISSION_IMMUTABLE, immutable);
	}

	public boolean isRename() {
		return isPermissionAllowed(PERMISSION_RENAME);
	}

	public void setRename(boolean rename) {
		setPermissionValue(PERMISSION_RENAME, rename);
	}

	public boolean isSecurity() {
		return isPermissionAllowed(PERMISSION_SECURITY);
	}

	public void setSecurity(boolean security) {
		setPermissionValue(PERMISSION_SECURITY, security);
	}

	public boolean isArchive() {
		return isPermissionAllowed(PERMISSION_ARCHIVE);
	}

	public void setArchive(boolean archive) {
		setPermissionValue(PERMISSION_ARCHIVE, archive);
	}

	public boolean isDownload() {
		return isPermissionAllowed(PERMISSION_DOWNLOAD);
	}

	public void setDownload(boolean download) {
		setPermissionValue(PERMISSION_DOWNLOAD, download);
	}

	public boolean isCalendar() {
		return isPermissionAllowed(PERMISSION_CALENDAR);
	}

	public void setCalendar(boolean calendar) {
		setPermissionValue(PERMISSION_CALENDAR, calendar);
	}

	public boolean isSubscription() {
		return isPermissionAllowed(PERMISSION_SUBSCRIPTION);
	}

	public void setSubscription(boolean subscription) {
		setPermissionValue(PERMISSION_SUBSCRIPTION, subscription);
	}

	public boolean isPrint() {
		return isPermissionAllowed(PERMISSION_PRINT);
	}

	public void setPrint(boolean print) {
		setPermissionValue(PERMISSION_PRINT, print);
	}

	public boolean isPassword() {
		return isPermissionAllowed(PERMISSION_PASSWORD);
	}

	public void setPassword(boolean password) {
		setPermissionValue(PERMISSION_PASSWORD, password);
	}

	public boolean isMove() {
		return isPermissionAllowed(PERMISSION_MOVE);
	}

	public void setMove(boolean move) {
		setPermissionValue(PERMISSION_MOVE, move);
	}

	public boolean isEmail() {
		return isPermissionAllowed(PERMISSION_EMAIL);
	}

	public void setEmail(boolean email) {
		setPermissionValue(PERMISSION_EMAIL, email);
	}

	public boolean isAutomation() {
		return isPermissionAllowed(PERMISSION_AUTOMATION);
	}

	public void setAutomation(boolean automation) {
		setPermissionValue(PERMISSION_AUTOMATION, automation);
	}

	public boolean isStorage() {
		return isPermissionAllowed(PERMISSION_STORAGE);
	}

	public void setStorage(boolean storage) {
		setPermissionValue(PERMISSION_STORAGE, storage);
	}

	public boolean isReadingreq() {
		return isPermissionAllowed(PERMISSION_READINGREQ);
	}

	public void setReadingreq(boolean readingreq) {
		setPermissionValue(PERMISSION_READINGREQ, readingreq);
	}

	public boolean isPermissionAllowed(String permission) {
		return getAllowedPermissions().contains(permission.toLowerCase());
	}

	public Set<String> getAllowedPermissions() {
		return permissions.stream().filter(e -> "true".equals(e.getValue())).map(e -> e.getCode().toLowerCase())
				.collect(Collectors.toSet());
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	@Override
	public String toString() {
		return getName();
	}
}