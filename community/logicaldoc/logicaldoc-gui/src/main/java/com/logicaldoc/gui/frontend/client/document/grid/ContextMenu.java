package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIExternalCall;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.PreviewPopup;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.ConversionDialog;
import com.logicaldoc.gui.frontend.client.document.DocumentCheckin;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.DownloadTicketDialog;
import com.logicaldoc.gui.frontend.client.document.EmailDialog;
import com.logicaldoc.gui.frontend.client.document.SendToArchiveDialog;
import com.logicaldoc.gui.frontend.client.document.SignatureDialog;
import com.logicaldoc.gui.frontend.client.document.StampDialog;
import com.logicaldoc.gui.frontend.client.document.WorkflowDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.AutoComplete;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
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
				if (selection.length == 1) {
					long id = selection[0].getId();
					DocUtil.download(id, null);
				} else {
					String url = GWT.getHostPageBaseURL() + "zip-export?folderId=" + folder.getId();
					for (GUIDocument record : selection) {
						if ("true".equals(record.getAttribute("password").toString())) {
							SC.warn(I18N.message("somedocsprotected"));
							break;
						}
						url += "&docId=" + record.getId();
					}
					WindowUtils.openUrl(url);
				}
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
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									grid.removeSelectedDocuments();
									DocumentController.get().deleted(grid.getSelectedDocuments());
									if (grid.getFolder() != null
											&& grid.getFolder().getId() == Session.get().getCurrentFolder().getId())
										DocumentsPanel.get().refresh();
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
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						Clipboard.getInstance().clear();
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
											Log.serverError(caught);
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
													Log.serverError(caught);
												}

												@Override
												public void onSuccess(Void result) {
													selection[0].setPasswordProtected(true);
													grid.updateDocument(selection[0]);
													Log.info("passwordapplied", null);
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
				if (Session.get().getUser().isMemberOf("admin")) {
					DocumentService.Instance.get().unsetPassword(selection[0].getId(), "", new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
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
														Log.serverError(caught);
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
									Log.serverError(caught);
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
						Log.serverError(caught);
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
				final GUIDocument document = grid.getSelectedDocument();
				DocumentService.Instance.get().checkout(document.getId(), new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GUIDocument[] docs = grid.getSelectedDocuments();
						for (GUIDocument record : docs){
							if (Session.get().getCurrentDocument() != null
									&& Session.get().getCurrentDocument().getId() == record.getId()) {
								DocUtil.markCheckedOut(Session.get().getCurrentDocument());
							} else {
								DocUtil.markCheckedOut(record);
							}
						}
						grid.selectDocument(document.getId());
						Log.info(I18N.message("documentcheckedout"), null);
						WindowUtils.openUrl(Util.downloadURL(document.getId()));
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
				DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument document) {
						DocumentCheckin checkin = new DocumentCheckin(document, filename);
						checkin.show();
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
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											grid.removeSelectedDocuments();
											Log.info(I18N.message("documentswerearchived", "" + selectionIds.length),
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
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GUIDocument[] selection = grid.getSelectedDocuments();
						for (GUIDocument record : selection) {
							record.setBookmarked(true);
							if (Session.get().getCurrentDocument() != null
									&& Session.get().getCurrentDocument().getId() == record.getId()) {
								Session.get().getCurrentDocument().setBookmarked(true);
								DocumentController.get().modified(Session.get().getCurrentDocument());
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
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (GUIDocument record : selection) {
							record.setIndexed(Constants.INDEX_SKIP);
							if (Session.get().getCurrentDocument() != null
									&& Session.get().getCurrentDocument().getId() == record.getId()) {
								Session.get().getCurrentDocument().setIndexed(Constants.INDEX_SKIP);
								DocumentController.get().modified(Session.get().getCurrentDocument());
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

				DocumentService.Instance.get().markIndexable(selectionIds, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						for (GUIDocument record : selection) {
							record.setIndexed(Constants.INDEX_TO_INDEX);
							if (Session.get().getCurrentDocument() != null
									&& Session.get().getCurrentDocument().getId() == record.getId()) {
								Session.get().getCurrentDocument().setIndexed(Constants.INDEX_TO_INDEX);
								DocumentController.get().modified(Session.get().getCurrentDocument());
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

				ContactingServer.get().show();
				DocumentService.Instance.get().indexDocuments(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						ContactingServer.get().hide();
						for (GUIDocument record : selection) {
							record.setIndexed(Constants.INDEX_INDEXED);
							if (Session.get().getCurrentDocument() != null
									&& Session.get().getCurrentDocument().getId() == record.getId()) {
								Session.get().getCurrentDocument().setIndexed(Constants.INDEX_INDEXED);
								DocumentController.get().modified(Session.get().getCurrentDocument());
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
				long docId = selection.getId();

				StampDialog dialog = new StampDialog(new long[] { docId });
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
				ListGrid list = (ListGrid) DocumentsPanel.get().getDocumentsGrid();
				ListGridRecord[] selection = list.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;

				final long[] ids = new long[selection.length];
				for (int j = 0; j < selection.length; j++) {
					ids[j] = Long.parseLong(selection[j].getAttribute("id"));
				}

				WorkflowDialog workflowDialog = new WorkflowDialog(ids);
				workflowDialog.show();
			}
		});

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				PreviewPopup iv = null;
				if (grid.getSelectedCount() == 1) {
					iv = new PreviewPopup(grid.getSelectedDocument());
				} else {
					iv = new PreviewPopup(grid.getSelectedDocuments(), 0);
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
											Log.serverError(caught);
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

		MenuItem more = new MenuItem(I18N.message("more"));

		MenuItem externalCall = new MenuItem();
		final GUIExternalCall extCall = Session.get().getSession().getExternalCall();
		if (extCall != null) {
			externalCall.setTitle(extCall.getName());
			externalCall.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
				public void onClick(MenuItemClickEvent event) {
					List<Long> ids = new ArrayList<Long>();
					List<String> filenames = new ArrayList<String>();
					for (GUIDocument record : selection) {
						ids.add(record.getId());
						filenames.add(record.getFileName());
					}

					WindowUtils.openUrl(
							extCall.getUrl(true, ids.toArray(new Long[0]), filenames.toArray(new String[0])),
							extCall.getTargetWindow() != null ? extCall.getTargetWindow() : "_blank", null);
				}
			});
		}

		setItems(download, preview, cut, copy, delete, bookmark, sendMail, links, office, checkout, checkin, lock,
				unlock);
		if (!Feature.visible(Feature.OFFICE))
			removeItem(office);
		if (Feature.visible(Feature.ARCHIVING))
			addItem(archive);
		if (Feature.enabled(Feature.EXTERNAL_CALL) && extCall != null)
			addItem(externalCall);

		addItem(more);

		Menu moreMenu = new Menu();
		moreMenu.setItems(indexSelection, markIndexable, markUnindexable, immutable, setPassword, unsetPassword,
				downloadTicket, replaceAlias);

		if (Feature.visible(Feature.FORMAT_CONVERSION))
			moreMenu.addItem(convert);
		if (Feature.visible(Feature.DIGITAL_SIGNATURE))
			moreMenu.addItem(sign);
		if (Feature.visible(Feature.STAMP))
			moreMenu.addItem(stamp);
		if (Feature.visible(Feature.IMPEX))
			moreMenu.addItem(sendToExpArchive);
		if (Feature.visible(Feature.WORKFLOW))
			moreMenu.addItem(startWorkflow);

		more.setSubmenu(moreMenu);

		/**
		 * Now implement the security policies
		 */
		{
			boolean someSelection = selection != null && selection.length > 0;
			boolean justOneSelected = someSelection && selection.length == 1;
			boolean immutablesInSelection = someSelection && checkImmutablesInSelection(selection);

			preview.setEnabled(someSelection);
			externalCall.setEnabled(someSelection);
			cut.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isMove());
			lock.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection));
			unlock.setEnabled(someSelection
					&& !immutablesInSelection
					&& (checkStatusInSelection(Constants.DOC_LOCKED, selection) || checkStatusInSelection(
							Constants.DOC_CHECKED_OUT, selection)));
			immutable.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
					&& folder.hasPermission(Constants.PERMISSION_IMMUTABLE));
			sign.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
					&& folder.hasPermission(Constants.PERMISSION_SIGN) && Feature.enabled(Feature.DIGITAL_SIGNATURE)
					&& Session.get().getUser().getCertDN() != null);
			stamp.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isWrite()
					&& Feature.enabled(Feature.STAMP));
			delete.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isDelete());
			links.setEnabled(!Clipboard.getInstance().isEmpty() && folder.isWrite());
			markIndexable.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isWrite());
			markUnindexable.setEnabled(someSelection && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection) && folder.isWrite());
			indexSelection
					.setEnabled(someSelection
							&& !immutablesInSelection
							&& (checkIndexedStatusInSelection(Constants.INDEX_INDEXED, selection) || checkIndexedStatusInSelection(
									Constants.INDEX_TO_INDEX, selection)));
			setPassword.setEnabled(justOneSelected && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
					&& folder.hasPermission(Constants.PERMISSION_PASSWORD) && !selection[0].isPasswordProtected());
			unsetPassword.setEnabled(justOneSelected && !immutablesInSelection
					&& checkStatusInSelection(Constants.DOC_UNLOCKED, selection)
					&& folder.hasPermission(Constants.PERMISSION_PASSWORD) && selection[0].isPasswordProtected());
			sendMail.setEnabled(someSelection && folder.hasPermission(Constants.PERMISSION_EMAIL));
			checkout.setEnabled(someSelection && !immutablesInSelection && folder.isDownload() && folder.isWrite()
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
		}
	}

	private boolean checkStatusInSelection(int status, GUIDocument[] selection) {
		for (GUIDocument doc : selection) {
			if (doc.getStatus() != status) {
				return false;
			} else if (status == Constants.DOC_CHECKED_OUT && doc.getLockUserId() != Session.get().getUser().getId()
					&& !Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)) {
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
}