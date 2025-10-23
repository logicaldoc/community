package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderSearchDialog;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Allows the selection of a specific folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderSelector extends TextItem {

	private Long folderId;

	private Collection<FolderChangeListener> listeners = new ArrayList<>();

	public FolderSelector(String name, List<FormItemIcon> additionalIcons) {
		this(name, false, additionalIcons);
	}

	public FolderSelector(String name, boolean allowRootSelection, List<FormItemIcon> additionalIcons) {
		if (name != null)
			setName(name);
		else
			setName("folder");
		setTitle(I18N.message("folder"));
		setWrapTitle(false);
		setValue("");
		setHintStyle("hint");

		if (additionalIcons != null && !additionalIcons.isEmpty())
			setWidth(180);

		Date date = new Date();
		Menu menu = new Menu();
		menu.setDataSource(new FoldersDS("folderselector" + date.getTime(), true, allowRootSelection, 100L));
		menu.setWidth(130);
		menu.setCanSelectParentItems(true);
		menu.setAutoFetchData(true);
		menu.addItemClickHandler(event -> {
			MenuItem item = event.getItem();
			setFolder(Long.parseLong(item.getAttributeAsString("folderId")), item.getAttributeAsString("name"));
		});

		FormItemIcon search = new FormItemIcon();
		search.setPrompt(I18N.message("search"));
		search.setSrc("[SKIN]/icons/magnifying-glass.png");
		search.setWidth(12);
		search.setHeight(12);
		search.addFormItemClickHandler(click -> new FolderSearchDialog(FolderSelector.this).show());

		PickerIcon pick = new PickerIcon(PickerIcon.COMBO_BOX, event -> menu.showContextMenu());

		FormItemIcon open = new FormItemIcon();
		open.setPrompt(I18N.message("openfolder"));
		open.setSrc("[SKIN]/icons/folder.png");
		open.setWidth(12);
		open.setHeight(12);
		open.addFormItemClickHandler(click -> {
			if (getFolderId() != null)
				DocumentsPanel.get().openInFolder(getFolderId(), null);
		});

		FormItemIcon clear = new FormItemIcon();
		clear.setPrompt(I18N.message("clear"));
		clear.setSrc("[SKIN]/icons/trash.png");
		clear.setWidth(12);
		clear.setHeight(12);
		clear.addFormItemClickHandler(click -> {
			clearValue();
			setFolder(null, null);
		});

		List<FormItemIcon> icons = new ArrayList<>();
		icons.add(pick);
		icons.add(search);
		icons.add(open);
		icons.add(clear);
		if (additionalIcons != null && !additionalIcons.isEmpty())
			icons.addAll(additionalIcons);
		setIcons(icons.toArray(new FormItemIcon[0]));

		addChangedHandler(event -> {
			ListGridRecord selection = getSelectedRecord();
			setFolder(Long.parseLong(selection.getAttribute("folderId")), selection.getAttribute("name"));
		});
	}

	public void setFolder(Long folderId, String folderName) {
		this.folderId = folderId;

		if (folderName != null && !(folderName.endsWith(" ") || folderName.endsWith("&nbsp;"))) {
			setValue(folderName);
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

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}