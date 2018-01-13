package com.logicaldoc.gui.frontend.client.clipboard;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Shows the clipboard content
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ClipboardWindow extends Window {

	private static ClipboardWindow instance = new ClipboardWindow();

	private ListGrid grid = new ListGrid();

	public ClipboardWindow() {
		super();

		HeaderControl trash = new HeaderControl(HeaderControl.TRASH, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord[] selection = grid.getSelection();
				if (selection == null || selection.length == 0) {
					Clipboard.getInstance().clear();
				} else {
					for (ListGridRecord record : selection) {
						GUIDocument doc = new GUIDocument();
						doc.setId(Long.parseLong(record.getAttribute("id")));
						Clipboard.getInstance().remove(doc);
					}
				}

				refresh();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, trash, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("clipboard"));
		setWidth(255);
		setHeight(200);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		grid = new ListGrid();
		grid.setWidth100();
		grid.setHeight100();
		grid.setCanReorderFields(false);
		grid.setCanFreezeFields(false);
		grid.setCanGroupBy(false);

		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField fileName = new ListGridField("filename", " ", 200);

		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");

		grid.setFields(new ListGridField[] { id, icon, fileName });
		grid.setCanResizeFields(true);
		addItem(grid);

		refresh();
	}

	private void refresh() {
		grid.setData(Clipboard.getInstance().getRecords());
	}

	public static ClipboardWindow getInstance() {
		return instance;
	}

	@Override
	public void show() {
		refresh();
		super.show();
	}

}