package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display usernames and optionally their avatars in cells that
 * contains a user reference
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class UserListGridField extends ColoredListGridField {

	boolean displayAvatar = true;

	boolean avoidCaching = false;

	private String avatarFieldName;

	public UserListGridField(String name) {
		this(name, null);
	}

	public UserListGridField(String name, String avatarFieldName, String title, boolean displayAvatar) {
		super(name, title != null ? I18N.message(title) : " ");
		this.displayAvatar = displayAvatar;
		this.avatarFieldName = avatarFieldName;
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter(new AvatarCellFormatter(16));
		setCellAlign(Alignment.LEFT);
		setAutoFitWidth(true);
	}

	public UserListGridField(String name, String avatarFieldName, String title) {
		super(name, title != null ? I18N.message(title) : " ");
		this.avatarFieldName = avatarFieldName;
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter(new AvatarCellFormatter(16));
		setCellAlign(Alignment.LEFT);
		setAutoFitWidth(true);
	}

	public UserListGridField(String name, String avatarFieldName) {
		this(name, avatarFieldName, null);
	}

	public UserListGridField() {
		this("avatar", null);
		setCanFilter(false);
		setCanSort(false);
	}

	public UserListGridField(boolean avoidCaching) {
		this();
		this.avoidCaching = avoidCaching;
	}

	public boolean isDisplayAvatar() {
		return displayAvatar;
	}

	public void setDisplayAvatar(boolean displayAvatar) {
		this.displayAvatar = displayAvatar;
	}

	public class AvatarCellFormatter extends ColoredCellFormatter {
		int size = Session.get().getConfigAsInt("gui.avatar.size");

		public AvatarCellFormatter() {
			super();
		}

		public AvatarCellFormatter(int size) {
			super();
			this.size = size;
		}

		@Override
		public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
			if (value == null)
				return "";

			String val = super.format(value, record, rowNum, colNum);

			Object avatarId = value;
			if (avatarFieldName != null)
				avatarId = record.getAttributeAsObject(avatarFieldName);

			if (avatarId == null || !UserListGridField.this.displayAvatar)
				return val != null ? val.toString() : "";

			String text = null;
			if (avatarFieldName != null)
				text = val != null ? val.toString() : null;

			if ("group".equals(avatarId)) {
				return AwesomeFactory.getIconHtml("user-friends") + (text != null && !text.isEmpty() ? " " + text : "");
			} else if ("attribute".equals(avatarId)) {
				return AwesomeFactory.getIconHtml("font") + (text != null && !text.isEmpty() ? " " + text : "");
			} else {
				if (text != null && !text.isEmpty())
					return Util.avatarWithText(avatarId.toString(), val);
				else {
					String url = Util.avatarUrl(avatarId.toString(), avoidCaching);
					return "<img src='" + url + "' style='border: 0px height: " + size + "px; width: " + size
							+ "px' />";
				}
			}
		}
	}
}