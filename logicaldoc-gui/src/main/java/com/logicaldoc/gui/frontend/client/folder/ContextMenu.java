package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gwt.http.client.RequestTimeoutException;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.GUIAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.ValuesCallback;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.document.grid.FillRoutineParams;
import com.logicaldoc.gui.frontend.client.folder.copy.FolderCopyDialog;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * This context menu is used for trees containing folders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class ContextMenu extends Menu {

	private FolderNavigator tree;

	private GUIAccessControlEntry acl;

	private List<GUIFolder> selectedFolders;

	public ContextMenu(FolderNavigator tree, GUIAccessControlEntry acl) {
		this.tree = tree;
		this.acl = acl;
		this.selectedFolders = tree.getSelectedFolders();

		GUIFolder firstSelectedFolder = selectedFolders.get(0);

		MenuItem search = prepareSearchMenuItem(firstSelectedFolder);

		MenuItem delete = prepareDeleteMenuItem();

		MenuItem create = prepareCreateMenuItem(firstSelectedFolder);

		MenuItem createAlias = prepareCreateAliasMenuItem();

		MenuItem rename = prepareRenameMenuItem();

		MenuItem createWorkspace = new MenuItem();
		createWorkspace.setTitle(I18N.message("newworkspace"));
		createWorkspace.addClickHandler(click -> onCreateWorkspace());

		MenuItem reload = new MenuItem();
		reload.setTitle(I18N.message("reload"));
		reload.addClickHandler(click -> tree.reload());

		MenuItem move = prepareMoveMenuItem();

		MenuItem copy = prepareCopyMenuItem();

		MenuItem merge = prepareMergeMenuItem();

		MenuItem paste = new MenuItem();
		paste.setTitle(I18N.message("paste"));
		paste.addClickHandler(click -> onPaste(firstSelectedFolder));

		MenuItem pasteAsAlias = new MenuItem();
		pasteAsAlias.setTitle(I18N.message("pasteasalias"));
		pasteAsAlias.addClickHandler(click -> onPasteAsAlias(firstSelectedFolder));

		MenuItem exportZip = prepareExportZipMenuItem(firstSelectedFolder);

		MenuItem addBookmark = new MenuItem();
		addBookmark.setTitle(I18N.message("addbookmark"));
		addBookmark.addClickHandler(click -> onAddBookmark(tree.getSelectedIds()));

		if (!acl.isWrite() || Clipboard.getInstance().isEmpty()) {
			paste.setEnabled(false);
			pasteAsAlias.setEnabled(false);
		}

		pasteAsAlias.setEnabled(!Clipboard.getInstance().getLastAction().equals(Clipboard.CUT));

		if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN) && Feature.visible(Feature.MULTI_WORKSPACE)) {
			setItems(reload, search, create, createAlias, rename, createWorkspace, delete, addBookmark, paste,
					pasteAsAlias, move, copy, merge, exportZip);
		} else {
			setItems(reload, search, create, createAlias, rename, delete, addBookmark, paste, pasteAsAlias, move, copy,
					merge, exportZip);
		}

		addSubscribeMenuItem();

		addApplyTemplateMenuItem();

		addArchiveMenuItem();

		addSendToExportArchiveMenuItem();

		addAutomationMenuItem();

		addCustomActionsMenuItem();

		if (selectedFolders.size() == 1)
			FolderService.Instance.get().getFolder(firstSelectedFolder.getId(), false, false, false,
					new GUIAsyncCallback<>() {
						@Override
						public void onSuccess(GUIFolder selectedFolder) {
							delete.setEnabled(!selectedFolder.isDefaultWorkspace());
							move.setEnabled(!selectedFolder.isDefaultWorkspace());
							rename.setEnabled(!selectedFolder.isDefaultWorkspace());
							createWorkspace.setEnabled(Feature.enabled(Feature.MULTI_WORKSPACE));
							merge.setEnabled(false);
						}
					});
	}

	private void addCustomActionsMenuItem() {
		if (Feature.enabled(Feature.CUSTOM_ACTIONS)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS)
				&& !Session.get().getUser().getCustomActions().isEmpty()) {
			MenuItem customActionsItem = prepareCustomActionsMenu();
			addItem(customActionsItem);
		}
	}

	private MenuItem prepareCustomActionsMenu() {
		Menu customActionsMenu = new Menu();
		for (GUIMenu menuAction : Session.get().getUser().getCustomActions())
			prepareCustomActionMenuItem(menuAction, customActionsMenu);

		MenuItem customActionsItem = new MenuItem(I18N.message("customactions"));
		customActionsItem.setSubmenu(customActionsMenu);
		return customActionsItem;
	}

	private void prepareCustomActionMenuItem(GUIMenu menuAction, Menu customActionsMenu) {
		MenuItem actionItem = new MenuItem(I18N.message(menuAction.getName()));
		customActionsMenu.addItem(actionItem);

		actionItem.addClickHandler(event ->
		/**
		 * Check on the server if the action has been modified
		 */
		SecurityService.Instance.get().getMenu(menuAction.getId(), I18N.getLocale(), new GUIAsyncCallback<>() {
			@Override
			public void onSuccess(GUIMenu action) {
				Session.get().getUser().updateCustomAction(action);

				if ((action.getRoutineId() == null || action.getRoutineId().longValue() == 0L)
						&& action.getAutomation() != null && !action.getAutomation().trim().isEmpty()) {
					/*
					 * An automation cript is specified directly, so launch it's
					 * execution
					 */
					GUIAutomationRoutine routine = new GUIAutomationRoutine();
					routine.setAutomation(action.getAutomation());
					executeRoutine(tree.getSelectedIds(), null, routine);
				} else if (action.getRoutineId() != null && action.getRoutineId().longValue() != 0L) {
					AutomationService.Instance.get().getRoutine(action.getRoutineId(), new GUIAsyncCallback<>() {
						@Override
						public void onSuccess(GUIAutomationRoutine routine) {
							if (routine.getTemplateId() != null && routine.getTemplateId().longValue() != 0L) {
								/*
								 * A routine with parameters is referenced, so
								 * open the input popup
								 */
								new FillRoutineParams(action.getName(), routine, tree.getSelectedIds(), null).show();
							} else {
								/*
								 * A routine without parameters is referenced,
								 * so launch directly
								 */
								executeRoutine(tree.getSelectedIds(), null, routine);
							}
						}
					});
				}
			}
		}));
	}

	private void executeRoutine(List<Long> folderIds, List<Long> docIds, GUIAutomationRoutine routine) {
		AutomationService.Instance.get().execute(routine, docIds, folderIds, new GUIAsyncCallback<>() {
			@Override
			public void onSuccess(Void arg0) {
				// Nothing to do
			}
		});
	}

	private void addAutomationMenuItem() {
		if (Feature.visible(Feature.AUTOMATION)) {
			MenuItem automation = new MenuItem();
			automation.setTitle(I18N.message("executeautomation"));
			automation.addClickHandler(eaClick -> new AutomationDialog(tree.getSelectedIds(), null).show());
			addItem(automation);
			automation.setEnabled(Feature.enabled(Feature.AUTOMATION) && acl.isAutomation());
		}
	}

	private void addSendToExportArchiveMenuItem() {
		if (Feature.visible(Feature.IMPEX)) {
			MenuItem sendToExpArchive = new MenuItem();
			sendToExpArchive.setTitle(I18N.message("sendtoexparchive"));
			sendToExpArchive.addClickHandler(
					event -> LD.ask(I18N.message("question"), I18N.message("confirmputinexparchive"), yes -> {
						if (Boolean.TRUE.equals(yes)) {
							new SendToArchiveDialog(tree.getSelectedIds(), false).show();
						}
					}));
			addItem(sendToExpArchive);
			sendToExpArchive.setEnabled(Feature.enabled(Feature.IMPEX) && acl.isExport());
		}
	}

	private void addArchiveMenuItem() {
		if (Feature.visible(Feature.ARCHIVING) && selectedFolders.size() == 1) {
			MenuItem archive = new MenuItem();
			archive.setTitle(I18N.message("archive"));
			archive.addClickHandler(archiveClick -> onArchive(selectedFolders.get(0).getId()));
			addItem(archive);
			archive.setEnabled(Feature.enabled(Feature.ARCHIVING) && acl.isArchive());
		}
	}

	private void addApplyTemplateMenuItem() {
		if (Feature.visible(Feature.FOLDER_TEMPLATE) && selectedFolders.size() == 1) {
			MenuItem applyTemplate = new MenuItem();
			applyTemplate.setTitle(I18N.message("applytemplate"));
			applyTemplate.addClickHandler(applyTemplateClick -> new ApplyTemplateDialog().show());
			addItem(applyTemplate);
			applyTemplate.setEnabled(Feature.enabled(Feature.FOLDER_TEMPLATE) && acl.isAdd());
		}
	}

	private void addSubscribeMenuItem() {
		if (Feature.visible(Feature.AUDIT) && selectedFolders.size() == 1) {
			MenuItem subscribe = new MenuItem();
			subscribe.setTitle(I18N.message("subscribe"));
			subscribe.addClickHandler(click -> new SubscriptionDialog(selectedFolders.get(0).getId(), null).show());
			subscribe.setEnabled(Feature.enabled(Feature.AUDIT));
			addItem(subscribe);
		}
	}

	private MenuItem prepareCopyMenuItem() {
		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(copyClick -> new FolderCopyDialog().show());
		return copy;
	}

	private MenuItem prepareCreateAliasMenuItem() {
		MenuItem createAlias = new MenuItem();
		createAlias.setTitle(I18N.message("createalias"));
		createAlias.addClickHandler(caClick -> new CreateAliasDialog().show());
		createAlias.setEnabled(
				acl.isAdd() && tree.getSelectedRecord().getAttributeAsString(FolderNavigator.FOLD_REF) == null);
		return createAlias;
	}

	private MenuItem prepareExportZipMenuItem(final GUIFolder folder) {
		MenuItem exportZip = new MenuItem();
		exportZip.setTitle(I18N.message("exportzip"));
		exportZip.addClickHandler(
				event -> Window.open(Util.contextPath() + "zip-export?folderId=" + folder.getId(), "_blank", ""));
		exportZip.setEnabled(acl.isExport() && acl.isDownload());
		return exportZip;
	}

	private MenuItem prepareMoveMenuItem() {
		MenuItem move = new MenuItem();
		move.setTitle(I18N.message("move"));
		move.addClickHandler(click -> new MoveDialog().show());
		move.setEnabled(acl.isMove());
		return move;
	}

	private MenuItem prepareMergeMenuItem() {
		MenuItem merge = new MenuItem();
		merge.setTitle(I18N.message("merge"));
		merge.addClickHandler(event -> new MergeDialog().show());
		merge.setEnabled(acl.isDelete());
		return merge;
	}

	private MenuItem prepareDeleteMenuItem() {
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> onDelete());
		delete.setEnabled(acl.isDelete());
		return delete;
	}

	private MenuItem prepareSearchMenuItem(GUIFolder folder) {
		MenuItem search = new MenuItem();
		search.setTitle(I18N.message("search"));
		search.addClickHandler(click -> {
			Search.get().getOptions().setFolder(folder.getId());
			Search.get().getOptions().setFolderName(folder.getName());
			Search.get().getOptions().setSearchInSubPath(false);
			Search.get().setOptions(Search.get().getOptions());
			MainPanel.get().selectSearchTab();
		});
		return search;
	}

	private MenuItem prepareRenameMenuItem() {
		MenuItem rename = new MenuItem();
		rename.setTitle(I18N.message("rename"));
		rename.addClickHandler(event -> onRename());
		rename.setEnabled(acl.isRename());
		return rename;
	}

	private MenuItem prepareCreateMenuItem(final GUIFolder folder) {
		MenuItem create = new MenuItem();
		create.setTitle(I18N.message("newfolder"));

		GUIFolder newFolder = new GUIFolder();
		newFolder.setParentId(folder.getId());
		create.addClickHandler(event -> new CreateDialog(newFolder).show());
		create.setEnabled(acl.isAdd());
		return create;
	}

	private void onDelete() {
		final List<Long> selectedIds = tree.getSelectedIds();
		LD.contactingServer();
		DocumentService.Instance.get().countDocuments(selectedIds, Constants.DOC_ARCHIVED, new GUIAsyncCallback<>() {
			@Override
			public void onSuccess(Long count) {
				LD.clearPrompt();
				final String folderMessage = selectedIds.size() == 1 ? "confirmdeletefolder" : "confirmdeletefolders";
				final String documentMessage = selectedIds.size() == 1 ? "confirmdeletefolderarchdocs"
						: "confirmdeletefoldersarchdocs";
				LD.ask(I18N.message("question"),
						count.longValue() == 0L ? (I18N.message(folderMessage)) : (I18N.message(documentMessage)),
						yes -> {
							if (Boolean.TRUE.equals(yes)) {
								LD.contactingServer();
								doDelete(selectedIds);
							}
						});
			}
		});
	}

	private void doDelete(final List<Long> selectedIds) {
		TreeNode parentNode = tree.getTree().getParent(tree.getSelectedRecord());
		TreeNode firstNode = tree.getTree().getChildren(parentNode)[0];

		FolderService.Instance.get().delete(selectedIds, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();

				if (caught instanceof RequestTimeoutException)
					SC.say("timeout");

				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				LD.clearPrompt();

				for (long id : selectedIds) {
					TreeNode node = tree.getTree().find(
							com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID, Long.toString(id));
					if (node != null)
						tree.getTree().remove(node);
				}

				if (parentNode == null || "/".equals(tree.getTree().getPath(parentNode))) {
					// In case of a workspace we close the whole tree and select
					// first workspace
					tree.getTree().closeAll();
					tree.selectFolder(Long.parseLong(firstNode.getAttributeAsString(
							com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID)));
					tree.getTree().openFolder(firstNode);
					selectRecord(0);
				} else {
					tree.selectFolder(Long.parseLong(parentNode.getAttributeAsString(
							com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID)));
					tree.reloadParentsOfSelection();
				}
			}
		});
	}

	private void onRename() {
		final TreeNode selectedNode = tree.getSelectedRecord();
		LD.askForValue(I18N.message("rename"), I18N.message("name"), selectedNode.getAttributeAsString("name"),
				value -> {
					if (value == null || "".equals(value.trim()))
						return;
					final String val = value.trim().replace("/", "").replace("\\\\", "");
					final long folderId = Long.parseLong(selectedNode.getAttributeAsString(
							com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID));
					FolderService.Instance.get().rename(folderId, val, new GUIAsyncCallback<>() {
						@Override
						public void onSuccess(Void v) {
							selectedNode.setAttribute("name", val);
							tree.refreshRow(getRecordIndex(selectedNode));
						}
					});
				});
	}

	private void onCreateWorkspace() {
		GUIFolder folder = new GUIFolder();
		folder.setType(1);
		new CreateDialog(folder).show();
	}

	private void onPaste(GUIFolder folder) {
		final long folderId = folder.getId();

		List<FormItem> items = new ArrayList<>();
		CheckboxItem copyDocuments = ItemFactory.newCheckbox("copydocuments");
		copyDocuments.setValue(true);
		copyDocuments.setDisabled(true);
		items.add(copyDocuments);

		CheckboxItem copyLinks = ItemFactory.newCheckbox("copylinks");
		copyLinks.setValue(true);
		items.add(copyLinks);

		CheckboxItem copyNotes = ItemFactory.newCheckbox("copynotes");
		copyNotes.setValue(true);
		items.add(copyNotes);

		CheckboxItem copySecuerity = ItemFactory.newCheckbox("copysecurity");
		copySecuerity.setValue(true);
		items.add(copySecuerity);

		LD.askForValues(I18N.message("copyoptions"), null, items, null, new ValuesCallback() {

			@Override
			public void execute(Map<String, Object> values) {
				FolderService.Instance.get().paste(
						Clipboard.getInstance().stream().map(doc -> doc.getId()).collect(Collectors.toList()), folderId,
						Clipboard.getInstance().getLastAction(), Boolean.TRUE.equals(values.get("copylinks")),
						Boolean.TRUE.equals(values.get("copynotes")), Boolean.TRUE.equals(values.get("copysecurity")),
						new GUIAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								DocumentsPanel.get().onFolderSelected(FolderController.get().getCurrentFolder());
								Clipboard.getInstance().clear();
							}
						});
			}

			@Override
			public void execute(String value) {
				// Not used
			}
		});
	}

	private void onPasteAsAlias(GUIFolder folder) {
		final long folderId = folder.getId();
		final List<Long> docIds = Clipboard.getInstance().stream().map(d -> d.getId()).collect(Collectors.toList());

		if (Feature.enabled(Feature.PDF))
			LD.askForValue(I18N.message("pasteasalias"), "type", "", ItemFactory.newAliasTypeSelector(),
					type -> pasteAsAlias(folderId, docIds, type));
		else
			pasteAsAlias(folderId, docIds, null);
	}

	private void pasteAsAlias(final long folderId, final List<Long> docIds, String type) {
		FolderService.Instance.get().pasteAsAlias(docIds, folderId, type, new GUIAsyncCallback<>() {
			@Override
			public void onSuccess(Void result) {
				DocumentsPanel.get().onFolderSelected(FolderController.get().getCurrentFolder());
				Clipboard.getInstance().clear();
				GuiLog.debug("Paste as Alias operation completed.");
			}
		});
	}

	/**
	 * Adds a bookmark to the currently selected folders
	 */
	private void onAddBookmark(List<Long> selection) {
		DocumentService.Instance.get().addBookmarks(selection, 1, new GUIAsyncCallback<>() {
			@Override
			public void onSuccess(Void v) {
				// Nothing to do
			}
		});
	}

	private void onArchive(final long folderId) {
		LD.askForValue(I18N.message("warning"), I18N.message("archiveadvice"), "", 400, value -> {
			if (value == null)
				return;

			if (value.isEmpty())
				SC.warn(I18N.message("commentrequired"));
			else
				DocumentService.Instance.get().archiveFolder(folderId, value, new GUIAsyncCallback<Long>() {
					@Override
					public void onSuccess(Long result) {
						GuiLog.info(I18N.message("documentswerearchived", "" + result), null);
						tree.reload();
					}
				});
		});
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ContextMenu)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}