package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A filed to display avatars in cells that contains a user reference
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class AvatarListGridField extends ListGridField {

	boolean avoidCaching = false;

	private String avatarFieldName;

	public AvatarListGridField(String name) {
		this(name, null);
	}

	public AvatarListGridField(String name, String avatarFieldName, String title, int width) {
		super(name, title != null ? I18N.message(title) : " ", width);
		this.avatarFieldName = avatarFieldName;
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter(new AvatarCellFormatter(16));
		setCellAlign(Alignment.LEFT);
	}

	public AvatarListGridField(String name, String avatarFieldName, String title) {
		this(name, avatarFieldName, title, 24);
	}

	public AvatarListGridField(String name, String avatarFieldName) {
		this(name, avatarFieldName, null);
	}

	public AvatarListGridField() {
		this("avatar", null);
		setCanFilter(false);
		setCanSort(false);
	}

	public AvatarListGridField(boolean avoidCaching) {
		this();
		this.avoidCaching = avoidCaching;
	}

	public class AvatarCellFormatter implements CellFormatter {
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
			Object avatarId = value;
			if (avatarFieldName != null)
				avatarId = record.getAttributeAsObject(avatarFieldName);

			if (avatarId == null)
				return value != null ? value.toString() : "";

			String text = null;
			if (avatarFieldName != null)
				text = value != null ? value.toString() : null;

			if ("group".equals(avatarId)) {
				return AwesomeFactory.getIconHtml("user-friends") + (text != null && !text.isEmpty() ? " " + text : "");
			} else if ("attribute".equals(avatarId)) {
				return AwesomeFactory.getIconHtml("font") + (text != null && !text.isEmpty() ? " " + text : "");
			} else {
				if (text != null && !text.isEmpty())
					return Util.avatarWithText(avatarId.toString(), value.toString());
				else {
					String url = Util.avatarUrl(avatarId.toString(), avoidCaching);
					return "<img src='" + url + "' style='border: 0px height: " + size + "px; width: " + size
							+ "px' />";
				}
			}
		}
	}
}