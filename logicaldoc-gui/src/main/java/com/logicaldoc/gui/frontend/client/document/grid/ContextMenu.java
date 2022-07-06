package com.logicaldoc.gui.frontend.client.document.grid;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.ComparisonWindow;
import com.logicaldoc.gui.frontend.client.document.ConversionDialog;
import com.logicaldoc.gui.frontend.client.document.DocumentCheckin;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.DownloadTicketDialog;
import com.logicaldoc.gui.frontend.client.document.EmailDialog;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.document.WorkflowDialog;
import com.logicaldoc.gui.frontend.client.document.signature.SignatureDialog;
import com.logicaldoc.gui.frontend.client.document.split.SplitDialog;
import com.logicaldoc.gui.frontend.client.document.stamp.StampDialog;
import com.logicaldoc.gui.frontend.client.folder.AutomationDialog;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.AutoComplete;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This context menu is used for grids containing document records.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ContextMenu extends Menu {

	public ContextMenu(final GUIFolder folder, final DocumentsGrid grid) {
		final GUIDocument[] selection = grid.getSelectedDocuments();
		final long[] selectionIds = grid.getSelectedIds();

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onDownload(folder, selection);
			}
		});

		MenuItem cut = new MenuItem();
		cut.setTitle(I18N.message("cut"));
		cut.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null)
					return;
				Clipboard.getInstance().clear();
				for (int i = 0; i < selection.length; i++) {
					long id = selection[i].getId();

					GUIDocument document = new GUIDocument();
					document.setId(id);
					document.setIcon(selection[i].getIcon());
					document.setLastModified(selection[i].getLastModified());
					document.setDate(selection[i].getDate());
					document.setVersion(selection[i].getVersion());
					document.setFileVersion(selection[i].getFileVersion());
					document.setFileName(selection[i].getFileName());

					Clipboard.getInstance().add(document);
					Clipboard.getInstance().setLastAction(Clipboard.CUT);
				}
			}
		});

		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null)
					return;
				Clipboard.getInstance().clear();
				for (int i = 0; i < selection.length; i++) {
					Clipboard.getInstance().add(selection[i]);
					Clipboard.getInstance().setLastAction(Clipboard.COPY);
				}
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;
				final long[] ids = new long[selection.length];
				for (int i = 0; i < selection.length; i++)
					ids[i] = selection[i].getId();

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							DocumentService.Instance.get().delete(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									// If the data grid is big the records
									// deletion doesn't work, so refresh the
									// screen.
									if (grid.getFolder() != null
											&& grid.getFolder().getId() == FolderController.get().getCurrentFolder().getId())
										DocumentsPanel.get().refresh();
									DocumentController.get().deleted(grid.getSelectedDocuments());
								}
							});
						}
					}
				});
			}
		});

		MenuItem sendMail = new MenuItem();
		sendMail.setTitle(I18N.message("sendmail"));
		sendMail.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				GUIDocument[] selection = grid.getSelectedDocuments();
				if (selection == null || selection.length < 1)
					return;
				EmailDialog window = new EmailDialog(grid.getSelectedIds(), selection[0].getFileName());
				window.show();
			}
		});

		MenuItem links = new MenuItem();
		links.setTitle(I18N.message("pasteaslinks"));
		links.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0 || Clipboard.getInstance().isEmpty())
					return;

				final long[] inIds = new long[Clipboard.getInstance().size()];
				int i = 0;
				for (GUIDocument doc : Clipboard.getInstance())
					inIds[i++] = doc.getId();

				DocumentService.Instance.get().linkDocuments(inIds, selectionIds, new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (GUIDocument doc : selection) {
							doc.setLinks(doc.getLinks() + 1);
							DocumentController.get().modified(doc);
						}

						for (GUIDocument doc : Clipboard.getInstance()) {
							doc.setLinks(doc.getLinks() + 1);
							DocumentController.get().modified(doc);
						}

						Clipboard.getInstance().clear();

						/**
						 * For some reason if the link target is already shown
						 * in the details panel it must be reloaded or further
						 * inputs will be lost.
						 */
						DocumentService.Instance.get().getById(grid.getSelectedDocument().getId(),
								new AsyncCallback<GUIDocument>() {
									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIDocument doc) {
										DocumentController.get().setCurrentDocument(doc);
									}
								});
					}
				});
			}
		});

		MenuItem immutable = new MenuItem();
		immutable.setTitle(I18N.message("makeimmutable"));
		immutable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				LD.askForValue(I18N.message("warning"), I18N.message("immutableadvice"), "", 600, new ValueCallback() {

					@Override
					public void execute(String value) {
						if (value == null)
							return;

						if (value.isEmpty())
							SC.warn(I18N.message("commentrequired"));
						else
							DocumentService.Instance.get().makeImmutable(selectionIds, value,
									new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											for (GUIDocument record : selection) {
												record.setImmutable(1);
												grid.updateDocument(record);
											}

											grid.selectDocument(selection[0].getId());
										}
									});
					}

				});
			}
		});

		MenuItem setPassword = new MenuItem();
		setPassword.setTitle(I18N.message("setpassword"));
		setPassword.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				TextItem password = ItemFactory.newPasswordItem("password", I18N.message("setpasswordwarning"), null);
				password.setAutoComplete(AutoComplete.NONE);
				LD.askForValue(I18N.message("setpassword"), I18N.message("setpasswordwarning"), "", password, 520,
						new ValueCallback() {

							@Override
							public void execute(String value) {
								if (value == null)
									return;

								if (value.isEmpty())
									SC.warn(I18N.message("passwordrequired"));
								else
									DocumentService.Instance.get().setPassword(selection[0].getId(), value,
											new AsyncCallback<Void>() {
												@Override
												public void onFailure(Throwable caught) {
													GuiLog.serverError(caught);
												}

												@Override
												public void onSuccess(Void result) {
													selection[0].setPasswordProtected(true);
													grid.updateDocument(selection[0]);
													GuiLog.info("passwordapplied", null);
												}
											});
							}

						});
			}
		});

		MenuItem unsetPassword = new MenuItem();
		unsetPassword.setTitle(I18N.message("unsetpassword"));
		unsetPassword.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (Session.get().isAdmin()) {
					DocumentService.Instance.get().unsetPassword(selection[0].getId(), "", new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							selection[0].setPasswordProtected(false);
							grid.updateDocument(selection[0]);
						}
					});
				} else
					LD.askForValue(I18N.message("unsetpassword"), I18N.message("currentpassword"), "", 300,
							new ValueCallback() {

								@Override
								public void execute(String value) {
									if (value == null)
										return;

									if (value.isEmpty())
										SC.warn(I18N.message("passwordrequired"));
									else
										DocumentService.Instance.get().unsetPassword(selection[0].getId(), value,
												new AsyncCallback<Void>() {
													@Override
													public void onFailure(Throwable caught) {
														GuiLog.serverError(caught);
													}

													@Override
													public void onSuccess(Void result) {
														selection[0].setPasswordProtected(false);
														grid.updateDocument(selection[0]);
													}
												});
								}

							});
			}
		});

		MenuItem lock = new MenuItem();
		lock.setTitle(I18N.message("lock"));
		lock.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				LD.askForValue(I18N.message("info"), I18N.message("lockadvice"), "", new ValueCallback() {

					@Override
					public void execute(String value) {
						if (value != null)
							DocumentService.Instance.get().lock(selectionIds, value, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									for (GUIDocument doc : selection)
										DocUtil.markLocked(doc);
									grid.selectDocument(selectionIds[0]);
								}
							});
					}

				});
			}
		});

		MenuItem unlock = new MenuItem();
		unlock.setTitle(I18N.message("unlock"));
		unlock.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null)
					return;

				DocumentService.Instance.get().unlock(selectionIds, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GUIDocument[] docs = grid.getSelectedDocuments();
						for (GUIDocument doc : docs)
							DocUtil.markUnlocked(doc);
						grid.selectDocument(selectionIds[0]);
					}
				});
			}
		});

		MenuItem checkout = new MenuItem();
		checkout.setTitle(I18N.message("checkout"));
		checkout.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				DocumentService.Instance.get().checkout(selectionIds, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GuiLog.info(I18N.message("documentcheckedout"), null);
						GUIDocument[] docs = grid.getSelectedDocuments();
						for (GUIDocument doc : docs)
							DocUtil.markCheckedOut(doc);
						grid.selectDocument(selectionIds[0]);
						onDownload(folder, selection);
					}
				});
			}
		});

		MenuItem checkin = new MenuItem();
		checkin.setTitle(I18N.message("checkin"));
		checkin.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				GUIDocument selection = grid.getSelectedDocument();
				if (selection == null)
					return;
				long id = selection.getId();
				final String filename = selection.getFileName();

				// Just to clean the upload folder
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
					}

					@Override
					public void onSuccess(Void result) {
						DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument document) {
								DocumentCheckin checkin = new DocumentCheckin(document, filename);
								checkin.show();
							}
						});
					}
				});
			}
		});

		MenuItem archive = new MenuItem();
		archive.setTitle(I18N.message("archive"));
		archive.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				LD.askForValue(I18N.message("warning"), I18N.message("archiveadvice"), "", 600, new ValueCallback() {

					@Override
					public void execute(String value) {
						if (value == null)
							return;

						if (value.isEmpty())
							SC.warn(I18N.message("commentrequired"));
						else
							DocumentService.Instance.get().archiveDocuments(selectionIds, value,
									new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											grid.removeSelectedDocuments();
											GuiLog.info(I18N.message("documentswerearchived", "" + selectionIds.length),
													null);
										}
									});
					}

				});
			}
		});

		MenuItem bookmark = new MenuItem();
		bookmark.setTitle(I18N.message("addbookmark"));
		bookmark.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;
				DocumentService.Instance.get().addBookmarks(selectionIds, 0, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GUIDocument[] selection = grid.getSelectedDocuments();
						for (GUIDocument record : selection) {
							record.setBookmarked(true);
							if (DocumentController.get().getCurrentDocument() != null
									&& DocumentController.get().getCurrentDocument().getId() == record.getId()) {
								DocumentController.get().getCurrentDocument().setBookmarked(true);
								DocumentController.get().modified(DocumentController.get().getCurrentDocument());
							} else {
								DocumentController.get().modified(record);
							}
						}
						DocumentsPanel.get().getDocumentsMenu().refresh("bookmarks");
					}
				});
			}
		});

		MenuItem markUnindexable = new MenuItem();
		markUnindexable.setTitle(I18N.message("markunindexable"));
		markUnindexable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				DocumentService.Instance.get().markUnindexable(selectionIds, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (GUIDocument record : selection) {
							record.setIndexed(Constants.INDEX_SKIP);
							if (DocumentController.get().getCurrentDocument() != null
									&& DocumentController.get().getCurrentDocument().getId() == record.getId()) {
								DocumentController.get().getCurrentDocument().setIndexed(Constants.INDEX_SKIP);
								DocumentController.get().modified(DocumentController.get().getCurrentDocument());
							} else {
								DocumentController.get().modified(record);
							}
						}
					}
				});
			}
		});

		MenuItem markIndexable = new MenuItem();
		markIndexable.setTitle(I18N.message("markindexable"));
		markIndexable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				DocumentService.Instance.get().markIndexable(selectionIds, Constants.INDEX_TO_INDEX,
						new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								for (GUIDocument record : selection) {
									record.setIndexed(Constants.INDEX_TO_INDEX);
									if (DocumentController.get().getCurrentDocument() != null
											&& DocumentController.get().getCurrentDocument().getId() == record.getId()) {
										DocumentController.get().getCurrentDocument().setIndexed(Constants.INDEX_TO_INDEX);
										DocumentController.get().modified(DocumentController.get().getCurrentDocument());
									} else {
										DocumentController.get().modified(record);
									}
								}
							}
						});
			}
		});

		MenuItem markIndexableMetadataOnly = new MenuItem();
		markIndexableMetadataOnly.setTitle(I18N.message("markindexablemetadataonly"));
		markIndexableMetadataOnly.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				DocumentService.Instance.get().markIndexable(selectionIds, Constants.INDEX_TO_INDEX_METADATA,
						new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								for (GUIDocument record : selection) {
									record.setIndexed(Constants.INDEX_TO_INDEX_METADATA);
									if (DocumentController.get().getCurrentDocument() != null
											&& DocumentController.get().getCurrentDocument().getId() == record.getId()) {
										DocumentController.get().getCurrentDocument()
												.setIndexed(Constants.INDEX_TO_INDEX_METADATA);
										DocumentController.get().modified(DocumentController.get().getCurrentDocument());
									} else {
										DocumentController.get().modified(record);
									}
								}
							}
						});
			}
		});

		MenuItem indexSelection = new MenuItem();
		indexSelection.setTitle(I18N.message("index"));
		indexSelection.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				Long[] ids = new Long[selectionIds.length];
				for (int i = 0; i < selectionIds.length; i++)
					ids[i] = selectionIds[i];

				LD.contactingServer();
				DocumentService.Instance.get().indexDocuments(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						LD.clearPrompt();
						for (GUIDocument record : selection) {
							record.setIndexed(Constants.INDEX_INDEXED);
							if (DocumentController.get().getCurrentDocument() != null
									&& DocumentController.get().getCurrentDocument().getId() == record.getId()) {
								DocumentController.get().getCurrentDocument().setIndexed(Constants.INDEX_INDEXED);
								DocumentController.get().modified(DocumentController.get().getCurrentDocument());
							} else {
								DocumentController.get().modified(record);
							}
						}
					}
				});
			}
		});

		MenuItem sign = new MenuItem();
		sign.setTitle(I18N.message("sign"));
		sign.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SignatureDialog dialog = new SignatureDialog(selectionIds);
				dialog.show();
			}
		});

		MenuItem stamp = new MenuItem();
		stamp.setTitle(I18N.message("stamp"));
		stamp.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				GUIDocument selection = grid.getSelectedDocument();
				if (selection == null)
					return;

				StampDialog dialog = new StampDialog(grid);
				dialog.show();
			}
		});

		MenuItem office = new MenuItem(I18N.message("editwithoffice"));
		office.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				GUIDocument selection = grid.getSelectedDocument();
				if (selection == null)
					return;
				WindowUtils.openUrl("ldedit:" + GWT.getHostPageBaseURL() + "ldedit?action=edit&sid="
						+ Session.get().getSid() + "&docId=" + selection.getId());
			}
		});

		MenuItem sendToExpArchive = new MenuItem(I18N.message("sendtoexparchive"));
		sendToExpArchive.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;

				SendToArchiveDialog archiveDialog = new SendToArchiveDialog(selectionIds, true);
				archiveDialog.show();
			}
		});

		MenuItem startWorkflow = new MenuItem(I18N.message("startworkflow"));
		startWorkflow.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				if (selectionIds.length < 1)
					return;

				WorkflowDialog workflowDialog = new WorkflowDialog(selectionIds);
				workflowDialog.show();
			}
		});

		MenuItem automation = new MenuItem(I18N.message("executeautomation"));
		automation.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				if (selectionIds.length < 1)
					return;

				Long[] ids = new Long[selectionIds.length];
				for (int i = 0; i < selectionIds.length; i++)
					ids[i] = Long.valueOf(selectionIds[i]);

				AutomationDialog dialog = new AutomationDialog(folder.getId(), ids);
				dialog.show();
			}
		});

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				PreviewPopup iv = null;

				if (grid.getSelectedCount() == 1) {
					GUIDocument doc = grid.getSelectedDocument();
					if (doc.getDocRef() != null) {
						/*
						 * in case of alias the data servlet inverts the docId
						 * and the docRef so in order to have the preview to do
						 * the right security checks we have to restore the
						 * correct ids
						 */
						long aliasId = doc.getDocRef();
						doc.setDocRef(doc.getId());
						doc.setId(aliasId);
					}
					iv = new PreviewPopup(doc);
				} else {
					GUIDocument[] docs = grid.getSelectedDocuments();
					for (GUIDocument doc : docs) {
						/*
						 * in case of alias the data servlet inverts the docId
						 * and the docRef so in order to have the preview to do
						 * the right security checks we have to restore the
						 * correct ids
						 */
						long aliasId = doc.getDocRef();
						doc.setDocRef(doc.getId());
						doc.setId(aliasId);
					}
					iv = new PreviewPopup(docs, 0);
				}
				iv.show();
			}
		});

		MenuItem downloadTicket = new MenuItem(I18N.message("downloadticket"));
		downloadTicket.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				GUIDocument selection = grid.getSelectedDocument();
				DownloadTicketDialog dialog = new DownloadTicketDialog(selection);
				dialog.show();
			}
		});

		MenuItem convert = new MenuItem(I18N.message("convert"));
		convert.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				GUIDocument selection = grid.getSelectedDocument();
				ConversionDialog dialog = new ConversionDialog(selection);
				dialog.show();
			}
		});

		MenuItem compare = new MenuItem(I18N.message("compare"));
		compare.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				GUIVersion version1 = new GUIVersion();
				version1.setDocId(selection[0].getId());
				version1.setFileVersion(selection[0].getFileVersion());
				version1.setFileName(selection[0].getFileName());

				GUIVersion version2 = new GUIVersion();
				version2.setDocId(selection[1].getId());
				version2.setFileVersion(selection[1].getFileVersion());
				version2.setFileName(selection[1].getFileName());

				ComparisonWindow diff = new ComparisonWindow(version1, version2);
				diff.show();
			}
		});

		MenuItem replaceAlias = new MenuItem(I18N.message("replacealias"));
		replaceAlias.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				final GUIDocument alias = grid.getSelectedDocument();
				LD.ask(I18N.message("replacealias"), I18N.message("replacealiasquestion"), new BooleanCallback() {

					@Override
					public void execute(Boolean value) {
						if (value.booleanValue())
							DocumentService.Instance.get().replaceAlias(alias.getId(),
									new AsyncCallback<GUIDocument>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(GUIDocument newDoc) {
											DocumentController.get().deleted(new GUIDocument[] { alias });
											DocumentController.get().stored(newDoc);
										}
									});
					}
				});
			}
		});

		Long[] ids = new Long[selectionIds.length];
		for (int i = 0; i < selectionIds.length; i++)
			ids[i] = Long.valueOf(selectionIds[i]);
		MenuItem customActionsItem = prepareCustomActionsMenu(folder.getId(), ids);

		MenuItem split = new MenuItem(I18N.message("split"));
		split.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				SplitDialog dialog = new SplitDialog(selection[0]);
				dialog.show();
			}
		});

		MenuItem merge = new MenuItem(I18N.message("merge"));
		merge.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				LD.askForStringMandatory(I18N.message("merge"), I18N.message("filename"), null, new ValueCallback() {

					@Override
					public void execute(String value) {
						LD.contactingServer();
						DocumentService.Instance.get().merge(selectionIds, folder.getId(), value,
								new AsyncCallback<GUIDocument>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
										LD.clearPrompt();
									}

									@Override
									public void onSuccess(GUIDocument mergedDoc) {
										LD.clearPrompt();
										DocumentController.get().stored(mergedDoc);
									}
								});
					}
				});
			}
		});

		setItems(download, preview, cut, copy, delete, bookmark, sendMail, links, office, checkout, checkin, lock,
				unlock);
		if (!Feature.visible(Feature.OFFICE))
			removeItem(office);
		if (Feature.visible(Feature.ARCHIVING))
			addItem(archive);
		if (Feature.enabled(Feature.CUSTOM_ACTIONS)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS)
				&& Session.get().getUser().getCustomActions() != null
				&& Session.get().getUser().getCustomActions().length > 0)
			addItem(customActionsItem);

		MenuItem more = new MenuItem(I18N.message("more"));
		addItem(more);

		Menu indexingMenu = new Menu();
		indexingMenu.setItems(indexSelection, markIndexable, markIndexableMetadataOnly, markUnindexable);
		MenuItem indexing = new MenuItem(I18N.message("indexing"));
		indexing.setSubmenu(indexingMenu);

		Menu moreMenu = new Menu();
		moreMenu.setItems(indexing, immutable, setPassword, unsetPassword, downloadTicket, replaceAlias);

		if (Feature.visible(Feature.FORMAT_CONVERSION))
			moreMenu.addItem(convert);
		if (Feature.visible(Feature.COMPARISON))
			moreMenu.addItem(compare);
		if (Feature.visible(Feature.DIGITAL_SIGNATURE))
			moreMenu.addItem(sign);
		if (Feature.visible(Feature.STAMP))
			moreMenu.addItem(stamp);
		if (Feature.visible(Feature.SPLIT))
			moreMenu.addItem(split);

		moreMenu.addItem(merge);

		if (Feature.visible(Feature.IMPEX))
			moreMenu.addItem(sendToExpArchive);
		if (Feature.visible(Feature.WORKFLOW))
			moreMenu.addItem(startWorkflow);
		if (Feature.visible(Feature.AUTOMATION))
			moreMenu.addItem(automation);

		more.setSubmenu(moreMenu);

		/**
		 * Now implement the security policies
		 */
		{
			boolean someSelection = selection != null && selection.length > 0;
			boolean moreSelected = selection != null && selection.length > 1;
			boolean justOneSelected = someSelection && selection.length == 1;
			boolean immutablesInSelection = someSelection && checkImmutablesInSelection(selection);

			preview.setEnabled(someSelection
					&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));
			cut.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isMove());
			unlock.setEnabled(
					someSelection && !immutablesInSelection && (checkStatusInSelection(Constants.DOC_LOCKED, selection)
							|| checkStatusInSelection(Constants.DOC_CHECKED_OUT, selection)));
			immutable.setEnabled(
					someSelection && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
							&& folder.hasPermission(Constants.PERMISSION_IMMUTABLE));
			sign.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
					&& folder.hasPermission(Constants.PERMISSION_SIGN) && Feature.enabled(Feature.DIGITAL_SIGNATURE)
					&& Session.get().getUser().getCertDN() != null);
			stamp.setEnabled(
					someSelection && !immutablesInSelection && checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
							&& folder.isWrite() && Feature.enabled(Feature.STAMP));
			delete.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isDelete());
			links.setEnabled(!Clipboard.getInstance().isEmpty() && folder.isWrite());
			markIndexable.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isWrite());
			markUnindexable.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isWrite());
			indexSelection.setEnabled(someSelection && !immutablesInSelection
					&& (checkIndexedStatusInSelection(Constants.INDEX_INDEXED, selection)
							|| checkIndexedStatusInSelection(Constants.INDEX_TO_INDEX, selection)
							|| checkIndexedStatusInSelection(Constants.INDEX_TO_INDEX_METADATA, selection))
					&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.INDEX));
			setPassword.setEnabled(justOneSelected && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
					&& folder.hasPermission(Constants.PERMISSION_PASSWORD) && !selection[0].isPasswordProtected());
			unsetPassword.setEnabled(justOneSelected && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
					&& folder.hasPermission(Constants.PERMISSION_PASSWORD) && selection[0].isPasswordProtected());
			sendMail.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_EMAIL));
			checkout.setEnabled(someSelection && !immutablesInSelection && folder.isDownload() && folder.isWrite()
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection));
			lock.setEnabled(someSelection && !immutablesInSelection && folder.isWrite()
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection));
			checkin.setEnabled(justOneSelected && !immutablesInSelection && folder.isWrite()
					&& checkStatusInSelection(Constants.DOC_CHECKED_OUT, selection));
			copy.setEnabled(someSelection);
			downloadTicket.setEnabled(justOneSelected && folder.isDownload());
			download.setEnabled(someSelection && folder.isDownload());
			office.setEnabled(justOneSelected && Feature.enabled(Feature.OFFICE)
					&& folder.hasPermission(Constants.PERMISSION_WRITE)
					&& folder.hasPermission(Constants.PERMISSION_DOWNLOAD)
					&& Util.isOfficeFile(grid.getSelectedDocument().getFileName()));
			convert.setEnabled(justOneSelected && Feature.enabled(Feature.FORMAT_CONVERSION));
			archive.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_ARCHIVE)
					&& Feature.enabled(Feature.ARCHIVING));
			sendToExpArchive.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_EXPORT)
					&& Feature.enabled(Feature.IMPEX));
			startWorkflow.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_WORKFLOW)
					&& Feature.enabled(Feature.WORKFLOW));
			replaceAlias.setEnabled(justOneSelected && folder.isWrite() && selection[0].getDocRef() != null);
			split.setEnabled(
					justOneSelected && selection[0].getFileName().toLowerCase().endsWith(".pdf") && folder.isWrite());
			merge.setEnabled(moreSelected && folder.isWrite());

			if ((selection != null && selection.length == 2) && Feature.enabled(Feature.COMPARISON)) {
				String fileName1 = selection[0].getFileName().toLowerCase();
				String fileName2 = selection[1].getFileName().toLowerCase();
				compare.setEnabled(Util.getExtension(fileName1).equalsIgnoreCase(Util.getExtension(fileName2)));
			} else
				compare.setEnabled(false);

			automation.setEnabled(Feature.enabled(Feature.AUTOMATION) && folder.hasPermission(Constants.PERMISSION_AUTOMATION));
		}
	}

	private MenuItem prepareCustomActionsMenu(final long folderId, final Long[] selectedDocIds) {
		Menu customActionsMenu = new Menu();
		if (Session.get().getUser().getCustomActions() != null
				&& Session.get().getUser().getCustomActions().length > 0) {
			for (GUIMenu menuAction : Session.get().getUser().getCustomActions()) {
				MenuItem actionItem = new MenuItem(I18N.message(menuAction.getName()));
				customActionsMenu.addItem(actionItem);

				actionItem.addClickHandler(new ClickHandler() {

					@Override
					public void onClick(MenuItemClickEvent event) {
						if (selectedDocIds == null || selectedDocIds.length < 1)
							return;

						/**
						 * Check on the server if the action has been modified
						 */
						SecurityService.Instance.get().getMenu(menuAction.getId(), I18N.getLocale(),
								new AsyncCallback<GUIMenu>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIMenu action) {
										Session.get().getUser().updateCustomAction(action);

										if ((action.getRoutineId() == null || action.getRoutineId().longValue() == 0L)
												&& action.getAutomation() != null
												&& !action.getAutomation().trim().isEmpty()) {
											/*
											 * An automation cript is specified
											 * directly, so launch it's
											 * execution
											 */
											GUIAutomationRoutine routine = new GUIAutomationRoutine();
											routine.setAutomation(action.getAutomation());
											executeRoutine(folderId, selectedDocIds, routine);
										} else if (action.getRoutineId() != null
												&& action.getRoutineId().longValue() != 0L) {
											AutomationService.Instance.get().getRoutine(action.getRoutineId(),
													new AsyncCallback<GUIAutomationRoutine>() {

														@Override
														public void onFailure(Throwable caught) {
															GuiLog.serverError(caught);
														}

														@Override
														public void onSuccess(GUIAutomationRoutine routine) {
															if (routine.getTemplateId() != null
																	&& routine.getTemplateId().longValue() != 0L) {
																/*
																 * A routine
																 * with
																 * parameters is
																 * referenced,
																 * so open the
																 * input popup
																 */
																FillRoutineParams dialog = new FillRoutineParams(
																		action.getName(), routine, folderId,
																		selectedDocIds);
																dialog.show();
															} else {
																/*
																 * A routine
																 * without
																 * parameters is
																 * referenced,
																 * so launch
																 * directly
																 */
																executeRoutine(folderId, selectedDocIds, routine);
															}
														}
													});
										}
									}
								});
					}
				});
			}
		}

		MenuItem customActionsItem = new MenuItem(I18N.message("customactions"));
		customActionsItem.setSubmenu(customActionsMenu);
		return customActionsItem;
	}

	private boolean checkStatusInSelection(int status, GUIDocument[] selection) {
		for (GUIDocument doc : selection) {
			if (doc.getStatus() != status) {
				return false;
			} else if (status == Constants.DOC_CHECKED_OUT && doc.getLockUserId() != Session.get().getUser().getId()
					&& !Session.get().isAdmin()) {
				return false;
			}
		}
		return true;
	}

	private boolean checkIndexedStatusInSelection(int status, GUIDocument[] selection) {
		for (GUIDocument doc : selection) {
			if (doc.getIndexed() != status)
				return false;
		}
		return true;
	}

	private boolean checkImmutablesInSelection(GUIDocument[] selection) {
		for (GUIDocument doc : selection)
			if (doc.getImmutable() != 0)
				return true;
		return false;
	}

	private void onDownload(final GUIFolder folder, final GUIDocument[] selection) {
		if (selection.length == 1) {
			long id = selection[0].getId();
			DocUtil.download(id, null);
		} else {
			String url = GWT.getHostPageBaseURL() + "zip-export?folderId=" + folder.getId();
			for (GUIDocument record : selection) {
				if (record.isPasswordProtected()) {
					SC.warn(I18N.message("somedocsprotected"));
					break;
				}
				url += "&docId=" + record.getId();
			}
			Util.download(url);
		}
	}

	private void executeRoutine(long folderId, Long[] docIds, GUIAutomationRoutine routine) {
		AutomationService.Instance.get().execute(routine, docIds, folderId, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
			}
		});
	}
}