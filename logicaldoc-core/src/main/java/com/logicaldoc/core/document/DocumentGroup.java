package com.logicaldoc.core.document;

import java.io.Serializable;

import com.logicaldoc.core.security.Permission;

/**
 * This class represents security permissions for a group in relation to a
 * document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.9.1
 */
public class DocumentGroup implements Serializable {

	private static final long serialVersionUID = 1L;

	private int read = 1;

	private int write = 0;

	private int download = 1;

	private int security = 0;

	private int delete = 0;

	private int rename = 0;

	private int immutable = 0;

	private int sign = 0;

	private int archive = 0;

	private int workflow = 0;

	private int calendar = 0;

	private int subscription = 0;

	private int password = 0;

	private int print = 1;

	private int move = 0;

	private int email = 1;

	private int automation = 0;

	private int readingreq = 0;

	private long groupId;

	public DocumentGroup(long groupId) {
		this.groupId = groupId;
	}

	public DocumentGroup() {
	}

	public DocumentGroup(DocumentGroup source) {
		super();
		this.read = source.read;
		this.write = source.write;
		this.download = source.download;
		this.security = source.security;
		this.delete = source.delete;
		this.rename = source.rename;
		this.immutable = source.immutable;
		this.sign = source.sign;
		this.archive = source.archive;
		this.workflow = source.workflow;
		this.calendar = source.calendar;
		this.subscription = source.subscription;
		this.password = source.password;
		this.print = source.print;
		this.move = source.move;
		this.email = source.email;
		this.automation = source.automation;
		this.readingreq = source.readingreq;
		this.groupId = source.groupId;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof DocumentGroup))
			return false;
		DocumentGroup other = (DocumentGroup) obj;
		return this.getGroupId() == other.getGroupId();
	}

	@Override
	public int hashCode() {
		return Long.valueOf(groupId).hashCode();
	}

	/**
	 * Parsing each permission and creates the integer representation
	 * 
	 * @return Permissions settings as integer representation.
	 */
	public int getPermissions() {
		/**
		 * Very important, see the Permission enumeration in order to replicate
		 * the same mask order.
		 */
		StringBuilder sb = new StringBuilder();
		sb.append(codeFlag(getReadingreq()));
		sb.append(codeFlag(getAutomation()));
		sb.append(codeFlag(getEmail()));
		sb.append(codeFlag(getMove()));
		sb.append(codeFlag(getPassword()));
		sb.append(codeFlag(getPrint()));
		sb.append(codeFlag(getSubscription()));
		sb.append(codeFlag(getCalendar()));
		sb.append(codeFlag(getDownload()));
		sb.append(codeFlag(getWorkflow()));
		sb.append(codeFlag(getArchive()));
		sb.append(codeFlag(getSign()));
		sb.append(codeFlag(getRename()));
		sb.append(codeFlag(getDelete()));
		sb.append(codeFlag(getImmutable()));
		sb.append(codeFlag(getSecurity()));
		sb.append(codeFlag(getWrite()));
		sb.append(codeFlag(getRead()));

		return Integer.parseInt(sb.toString(), 2);
	}

	private String codeFlag(int flagValue) {
		return flagValue == 1 ? "1" : "0";
	}

	/**
	 * Set each permission evaluating the given integer representation.
	 * 
	 * @param permissions mask(the last slot is for the 'read' permission and it
	 *        is not evaluated)
	 */
	public void setPermissions(int permissions) {
		setRead(decodeFlag(Permission.READ.match(permissions)));
		setWrite(decodeFlag(Permission.WRITE.match(permissions)));
		setSecurity(decodeFlag(Permission.SECURITY.match(permissions)));
		setImmutable(decodeFlag(Permission.IMMUTABLE.match(permissions)));
		setDelete(decodeFlag(Permission.DELETE.match(permissions)));
		setRename(decodeFlag(Permission.RENAME.match(permissions)));
		setSign(decodeFlag(Permission.SIGN.match(permissions)));
		setArchive(decodeFlag(Permission.ARCHIVE.match(permissions)));
		setWorkflow(decodeFlag(Permission.WORKFLOW.match(permissions)));
		setDownload(decodeFlag(Permission.DOWNLOAD.match(permissions)));
		setCalendar(decodeFlag(Permission.CALENDAR.match(permissions)));
		setSubscription(decodeFlag(Permission.SUBSCRIPTION.match(permissions)));
		setPrint(decodeFlag(Permission.PRINT.match(permissions)));
		setPassword(decodeFlag(Permission.PASSWORD.match(permissions)));
		setMove(decodeFlag(Permission.MOVE.match(permissions)));
		setEmail(decodeFlag(Permission.EMAIL.match(permissions)));
		setAutomation(decodeFlag(Permission.AUTOMATION.match(permissions)));
		setReadingreq(decodeFlag(Permission.READINGREQ.match(permissions)));
	}

	private int decodeFlag(boolean flagValue) {
		return flagValue ? 1 : 0;
	}

	public int getRead() {
		return read;
	}

	public void setRead(int read) {
		this.read = read;
	}

	public int getWrite() {
		return write;
	}

	public void setWrite(int write) {
		this.write = write;
	}

	public int getSecurity() {
		return security;
	}

	public void setSecurity(int security) {
		this.security = security;
	}

	public int getDelete() {
		return delete;
	}

	public void setDelete(int delete) {
		this.delete = delete;
	}

	public int getRename() {
		return rename;
	}

	public void setRename(int rename) {
		this.rename = rename;
	}

	public int getSign() {
		return sign;
	}

	public void setSign(int sign) {
		this.sign = sign;
	}

	public int getArchive() {
		return archive;
	}

	public void setArchive(int archive) {
		this.archive = archive;
	}

	public int getWorkflow() {
		return workflow;
	}

	public void setWorkflow(int workflow) {
		this.workflow = workflow;
	}

	public long getGroupId() {
		return groupId;
	}

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	public int getImmutable() {
		return immutable;
	}

	public void setImmutable(int immutable) {
		this.immutable = immutable;
	}

	public int getDownload() {
		return download;
	}

	public void setDownload(int download) {
		this.download = download;
	}

	public int getCalendar() {
		return calendar;
	}

	public void setCalendar(int calendar) {
		this.calendar = calendar;
	}

	public int getSubscription() {
		return subscription;
	}

	public void setSubscription(int subscription) {
		this.subscription = subscription;
	}

	public int getPrint() {
		return print;
	}

	public void setPrint(int print) {
		this.print = print;
	}

	public int getPassword() {
		return password;
	}

	public void setPassword(int password) {
		this.password = password;
	}

	public int getEmail() {
		return email;
	}

	public void setEmail(int email) {
		this.email = email;
	}

	public int getMove() {
		return move;
	}

	public void setMove(int move) {
		this.move = move;
	}

	public int getAutomation() {
		return automation;
	}

	public void setAutomation(int automation) {
		this.automation = automation;
	}

	public int getReadingreq() {
		return readingreq;
	}

	public void setReadingreq(int readingreq) {
		this.readingreq = readingreq;
	}
}