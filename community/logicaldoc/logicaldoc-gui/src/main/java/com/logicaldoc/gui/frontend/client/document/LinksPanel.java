package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.LinksDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * This panel shows the links of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class LinksPanel extends DocumentDetailTab {

	private TreeGrid treeGrid;

	public LinksPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		ListGridField type = new ListGridField("type", I18N.message("type"), 100);
		type.setCanEdit(true);

		ListGridField direction = new ListGridField("direction", I18N.message("direction"), 60);
		direction.setCanSort(false);
		direction.setType(ListGridFieldType.IMAGE);
		direction.setCanSort(false);
		direction.setAlign(Alignment.CENTER);
		direction.setShowDefaultContextMenu(false);
		direction.setImageURLPrefix(Util.imagePrefix() + "document_");
		direction.setImageURLSuffix(".png");
		direction.setCanEdit(false);

		ListGridField fileName = new ListGridField("filename", I18N.message("link"), 250);
		fileName.setCanEdit(false);

		GUIFolder folder = Session.get().getCurrentFolder();

		treeGrid = new TreeGrid() {

			@Override
			protected String getIcon(Record node, boolean defaultState) {
				return Util.imageUrl(node.getAttributeAsString("icon") + ".png");
			}

		};
		treeGrid.setCanEdit(false);
		treeGrid.setClosedIconSuffix("");
		treeGrid.setCanFreezeFields(true);
		treeGrid.setAutoFetchData(true);
		treeGrid.setTitleField("filename");
		treeGrid.setDataSource(new LinksDS(document.getId()));
		treeGrid.setAutoFetchData(true);
		treeGrid.setLoadDataOnDemand(true);
		treeGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		treeGrid.setFields(fileName, direction, type);
		addMember(treeGrid);

		if (folder != null && folder.hasPermission(Constants.PERMISSION_WRITE)) {
			treeGrid.setCanEdit(true);
			treeGrid.setEditEvent(ListGridEditEvent.CLICK);
			treeGrid.setEditByCell(true);
			treeGrid.addEditCompleteHandler(new EditCompleteHandler() {
				@Override
				public void onEditComplete(EditCompleteEvent event) {
					long id = Long.parseLong(event.getOldValues().getAttribute("linkId"));
					final String type = (String) event.getNewValues().get("type");
					DocumentService.Instance.get().updateLink(id, type, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							treeGrid.getSelectedRecord().setAttribute("type", type);
							treeGrid.updateData(treeGrid.getSelectedRecord());
						}
					});
				}
			});
		}

		treeGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				final Menu contextMenu = new Menu();

				final ListGridRecord[] selection = treeGrid.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;
				final long[] ids = new long[selection.length];
				for (int i = 0; i < selection.length; i++)
					ids[i] = Long.parseLong(selection[i].getAttribute("linkId"));

				final MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {

						LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
							@Override
							public void execute(Boolean value) {
								if (value) {
									DocumentService.Instance.get().deleteLinks(ids, new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											TreeNode parent = treeGrid.getTree()
													.getParent(treeGrid.getSelectedRecord());
											treeGrid.selectRecord(parent);
											treeGrid.getTree().reloadChildren(parent);
											document.setLinks(document.getLinks() - ids.length);
											DocumentController.get().modified(document);
										}
									});
								}
							}
						});
					}
				});

				final MenuItem preview = new MenuItem();
				preview.setTitle(I18N.message("preview"));
				preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						onPreview(treeGrid.getSelectedRecord());
					}
				});

				final MenuItem download = new MenuItem();
				download.setTitle(I18N.message("download"));
				download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						onDownload(treeGrid.getSelectedRecord());
					}
				});

				final MenuItem downloadPackage = new MenuItem();
				downloadPackage.setTitle(I18N.message("downloadpackage"));
				downloadPackage.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						onDownloadPackage();
					}
				});

				final MenuItem openInFolder = new MenuItem();
				openInFolder.setTitle(I18N.message("openinfolder"));
				openInFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						onOpenInFolder(treeGrid.getSelectedRecord());
					}
				});

				// Compute the folder ID of the referenced document
				long folderId = selection[0].getAttributeAsLong("folderId1");
				if (document.getFolder().getId() == folderId)
					folderId = selection[0].getAttributeAsLong("folderId2");

				FolderService.Instance.get().getFolder(folderId, false, false, false, new AsyncCallback<GUIFolder>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder fld) {
						if (fld == null)
							return;
						download.setEnabled(fld.isDownload());
						downloadPackage.setEnabled(fld.isDownload());
						delete.setEnabled(fld.isDelete());
						openInFolder.setEnabled(com.logicaldoc.gui.common.client.Menu
								.enabled(com.logicaldoc.gui.common.client.Menu.DOCUMENTS));
						contextMenu.setItems(preview, download, downloadPackage, openInFolder, delete);
						contextMenu.showContextMenu();
					}
				});

				event.cancel();
			}
		});

		treeGrid.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				treeGrid.addCellDoubleClickHandler(new CellDoubleClickHandler() {
					@Override
					public void onCellDoubleClick(CellDoubleClickEvent event) {
						final ListGridRecord record = event.getRecord();

						FolderService.Instance.get().getFolder(record.getAttributeAsLong("folderId"), false, false,
								false, new AsyncCallback<GUIFolder>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIFolder fld) {
										if (fld.isDownload() && "download"
												.equals(Session.get().getInfo().getConfig("gui.doubleclick")))
											onDownload(record);
										else
											onPreview(record);
									}
								});
					}
				});
			}
		});
	}

	protected void onOpenInFolder(ListGridRecord record) {
		String documentId = record.getAttributeAsString("documentId");
		long docId = Long.parseLong(documentId.substring(documentId.lastIndexOf('-') + 1));
		DocumentService.Instance.get().getById(docId, new AsyncCallback<GUIDocument>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught.getMessage(), caught);
			}

			@Override
			public void onSuccess(GUIDocument document) {
				DocumentsPanel.get().openInFolder(document.getFolder().getId(), document.getId());
			}
		});
	}

	protected void onDownload(ListGridRecord record) {
		if (document.getFolder().isDownload()) {
			String documentId = record.getAttributeAsString("documentId");
			long docId = Long.parseLong(documentId.substring(documentId.lastIndexOf('-') + 1));
			DocUtil.download(docId, null);
		}
	}

	protected void onDownloadPackage() {
		if (document.getFolder().isDownload()) {
			String url = GWT.getHostPageBaseURL() + "zip-export?folderId=" + document.getFolder().getId();
			url += "&docId=" + document.getId();

			treeGrid.getRecords();

			for (ListGridRecord record : treeGrid.getRecords()) {
				if (record.getAttributeAsBoolean("password")) {
					SC.warn(I18N.message("somedocsprotected"));
					break;
				}

				String docId = record.getAttribute("documentId");
				docId = docId.substring(docId.indexOf('-') + 1);
				url += "&docId=" + docId;
			}
			WindowUtils.openUrl(url);
		}
	}

	protected void onPreview(ListGridRecord record) {
		String documentId = record.getAttributeAsString("documentId");
		long docId = Long.parseLong(documentId.substring(documentId.lastIndexOf('-') + 1));
		DocumentService.Instance.get().getById(docId, new AsyncCallback<GUIDocument>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught.getMessage(), caught);
			}

			@Override
			public void onSuccess(GUIDocument document) {
				PreviewPopup iv = new PreviewPopup(document);
				iv.show();
			}
		});
	}
}