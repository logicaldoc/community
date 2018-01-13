package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ItemClickEvent;
import com.smartgwt.client.widgets.menu.events.ItemClickHandler;

/**
 * Allows the selection of a specific folder
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class FolderSelector extends TextItem {

	private Long folderId;

	private Menu menu = new Menu();

	private Collection<FolderChangeListener> listeners = new ArrayList<FolderChangeListener>();

	public FolderSelector(String name, boolean clean) {
		if (name != null)
			setName(name);
		else
			setName("folder");
		setTitle(I18N.message("folder"));
		setWrapTitle(false);
		setValue("");
		setHintStyle("hint");
		
        Date date = new Date();
		menu.setDataSource(new FoldersDS("folderselector" + date.getTime()));
		menu.setWidth(130);
		menu.setCanSelectParentItems(true);
        menu.setAutoFetchData(true);
		menu.addItemClickHandler(new ItemClickHandler() {
			public void onItemClick(ItemClickEvent event) {
				MenuItem item = event.getItem();
				setFolder(new Long(item.getAttributeAsString("folderId")), item.getAttributeAsString("name"));
			}
		});

		PickerIcon search = new PickerIcon(PickerIcon.SEARCH, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				FolderSearchDialog dialog = new FolderSearchDialog(FolderSelector.this);
				dialog.show();
			}
		});

		PickerIcon pick = new PickerIcon(PickerIcon.COMBO_BOX, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				menu.showContextMenu();
			}
		});

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				clearValue();
				setFolder(null, null);
			}
		});

		setIcons(pick, search, clear);

		addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selection = getSelectedRecord();
				setFolder(Long.parseLong(selection.getAttribute("folderId")), selection.getAttribute("name"));
			}
		});
	}

	public void setFolder(Long folderId, String name) {
		this.folderId = folderId;

		if (name != null && !(name.endsWith(" ") || name.endsWith("&nbsp;"))) {
			setValue(name);
		} else {
			setValue("");
		}

		for (FolderChangeListener listener : listeners) {
			listener.onChanged(getFolder());
		}
	}

	public void setFolder(GUIFolder folder) {
		Long id = null;
		String name = null;

		if (folder != null) {
			id = folder.getId();
			if (id == 5)
				name = "/";
			else
				name = folder.getName();
		}

		setFolder(id, name);
	}

	public GUIFolder getFolder() {
		if (getFolderId() != null) {
			GUIFolder folder = new GUIFolder();
			folder.setId(getFolderId());
			folder.setName(getFolderName());
			return folder;
		} else
			return null;
	}

	public Long getFolderId() {
		return folderId;
	}

	public String getFolderName() {
		return (String) getValue();
	}

	public void addFolderChangeListener(FolderChangeListener listener) {
		listeners.add(listener);
	}
}