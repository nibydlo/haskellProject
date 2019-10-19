{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module Main where

import Client (deleteNoteRequest, getNotes, getNote, postNote, putNote)
import GHC.Int (Int32(..))
import qualified Data.Text.Lazy as Lazy (pack, toStrict, Text(..))
import Domain (id, title, text, Note(..))
import Data.Text (pack, unpack, Text(..))
import qualified GI.Gtk as GI (init, main)
import GI.Gtk.Objects.Widget (widgetSetMarginLeft)
import GI.Gtk (
                buttonNew
              , castTo
              , containerAdd
              , containerGetChildren
              , containerRemove
              , containerSetBorderWidth
              , drawingAreaNew
              , entryGetText
              , entryNew
              , entrySetMaxLength
              , entrySetText
              , flowBoxNew
              , flowBoxSetMaxChildrenPerLine
              , flowBoxSetSelectionMode
              , getTextViewBuffer
              , headerBarNew
              , headerBarSetTitle
              , headerBarSetSubtitle
              , headerBarSetShowCloseButton
              , labelGetText
              , labelSetText
              , mainQuit
              , new
              , on
              , onWidgetDestroy
              , scrolledWindowNew
              , scrolledWindowSetPolicy
              , setWidgetWidthRequest
              , textBufferNew
              , textBufferGetStartIter
              , textBufferGetEndIter
              , textBufferGetText
              , textBufferSetText
              , textViewGetBuffer
              , textViewNew
              , textViewSetBuffer
              , toWidget
              , widgetSetValign
              , widgetHide
              , widgetShowAll
              , widgetSetMarginBottom
              , widgetSetMarginTop
              , widgetSetSizeRequest
        --       , widgetOverrideBackgroundColor
        --       , widgetOverrideColor
              , windowNew
              , Orientation (OrientationVertical)
              , AttrOp( (:=) )
              , Box(..)
              , Button(..)
              , Entry(..)
              , FlowBox(..)
              , Label(..)
              , ScrolledWindow(..)
              , TextView(..)
              , Widget(..)
            )
import GI.Gtk.Objects.Window (windowSetDefaultSize, windowSetTitle, Window(..))
import GI.Gtk.Objects.Button (setButtonLabel)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Alignment (alignmentNew)
import GI.Gtk.Enums (WindowType(..), PolicyType(..),Align(..),SelectionMode(..))
import Utils (cutTitle, getMaxId, numBytesUtf8, toLazy)

textViewGetValue :: TextView -> IO Text
textViewGetValue tv = do
  buf <- textViewGetBuffer tv
  start <- textBufferGetStartIter buf
  end <- textBufferGetEndIter buf
  value <- textBufferGetText buf start end True
  return value

getTVText :: Widget -> IO Text
getTVText w = do
  maybeTv <- castTo TextView w
  case maybeTv of
    Nothing -> return ""
    Just tv -> textViewGetValue tv

getTV :: Widget -> IO TextView
getTV w = do
  maybeTv <- castTo TextView w
  case maybeTv of
    Nothing -> do
      newTV <- textViewNew
      return newTV
    Just tv -> return tv

openNote :: String -> FlowBox -> Label -> Entry -> TextView -> IO ()
openNote noteIdString fb stateLabel entry textView = do
  let noteId = read noteIdString :: Integer
  note <- getNote noteId
  labelSetText stateLabel $ pack noteIdString -- update currently open note id
  entrySetText entry (Lazy.toStrict $ title note) -- update title of note
  widgetShowAll entry -- show new title
  oldTextBuffer <- getTextViewBuffer textView --
  print (Lazy.toStrict $ Domain.text note)
  textBufferSetText oldTextBuffer (Lazy.toStrict $ Domain.text note) ((fromIntegral ((numBytesUtf8 $ unpack $ Lazy.toStrict $ Domain.text note) :: Int) :: Int32))
  textViewSetBuffer textView (Just oldTextBuffer) -- update text of note
  widgetShowAll textView -- show new text
  return ()

createNoteButton :: Note -> FlowBox -> Label -> Entry -> TextView  -> IO Box
createNoteButton note fb stateLabel entry textView = do
  noteButton <- new Button [#label := (cutTitle $ Lazy.toStrict $ title note)]
  widgetSetSizeRequest noteButton 150 40
  noteBox <- new Box [#expand := False]
  #add noteBox noteButton
  widgetSetMarginTop noteBox 10
  on noteButton #clicked (openNote (show $ Domain.id note) fb stateLabel entry textView)
  return noteBox

createFlowbox :: FlowBox -> Label -> Entry -> TextView -> IO ()
createFlowbox fb stateLabel entry textView = do
  children <- containerGetChildren fb
  mapM_ (containerRemove fb) children
  notes <- getNotes
  mapM_ (\note -> (createNoteButton note fb stateLabel entry textView >>= (\b -> containerAdd fb b)) ) notes
  widgetShowAll fb

changeTV :: ScrolledWindow -> IO ()
changeTV window = do
  children <- containerGetChildren window
  mapM_ (containerRemove window) children

createNote :: Entry -> ScrolledWindow -> Label -> FlowBox -> IO ()
createNote titleEntry noteTextScrolled stateLabel listFlowBox = do
  entrySetText titleEntry ""
  changeTV noteTextScrolled
  textView <- textViewNew
  containerAdd noteTextScrolled textView
  widgetShowAll noteTextScrolled
  notes <- getNotes
  let newId = 1 + (getMaxId notes)
  let newNote = Note newId "newNote" ""
  postNote newNote
  labelSetText stateLabel $ pack (show newId)
  createFlowbox listFlowBox stateLabel titleEntry textView
  return ()

saveNote :: Entry -> ScrolledWindow -> Label -> FlowBox -> TextView -> IO ()
saveNote titleEntry noteTextScrolled stateLabel listFlowBox tv = do
  children <- containerGetChildren noteTextScrolled
  let textView = Prelude.head children
  noteTitle <- entryGetText titleEntry
  noteText <- getTVText textView
  noteIdText <- labelGetText stateLabel
  let noteId = read (unpack noteIdText) :: Integer
  putNote (Note noteId (toLazy noteTitle) (toLazy noteText))
  actualTV <- getTV textView
  createFlowbox listFlowBox stateLabel titleEntry actualTV

deleteNote :: FlowBox -> Label -> Entry -> ScrolledWindow -> IO ()
deleteNote fb stateLabel titleEntry noteTextScrolled = do
  entrySetText titleEntry ""
  changeTV noteTextScrolled
  textView <- textViewNew
  containerAdd noteTextScrolled textView
  widgetHide titleEntry
  widgetHide textView
  noteIdText <- labelGetText stateLabel
  deleteNoteRequest  (read (unpack noteIdText) :: Integer)
  createFlowbox fb stateLabel titleEntry textView

mainWindow :: IO Window
mainWindow = do
  win <- windowNew WindowTypeToplevel
  windowSetTitle win "Notes"
  containerSetBorderWidth win 10
  windowSetDefaultSize win 600 480

  noteListBox <- new Box [#orientation := OrientationVertical, #expand := False]

  stateLabel <- new Label [#label := "1"]
  widgetHide stateLabel

  updateButton <- new Button [#label := "Reload list"]
  widgetSetSizeRequest updateButton 150 40

  updateButtonBox <- new Box [#expand := False]
  widgetSetMarginTop updateButtonBox 10
  widgetSetMarginBottom updateButtonBox 15
  #add updateButtonBox updateButton
  #add noteListBox updateButtonBox

  noteList <- scrolledWindowNew noAdjustment noAdjustment
  widgetSetSizeRequest noteList 120 340
  scrolledWindowSetPolicy noteList PolicyTypeNever PolicyTypeAutomatic
  #add noteListBox noteList
  mainBox <- new Box [#expand := False]
  #add mainBox noteListBox
  #add win mainBox

  listFlowBox <- flowBoxNew
  widgetSetValign listFlowBox AlignStart
  flowBoxSetMaxChildrenPerLine listFlowBox 1
  flowBoxSetSelectionMode listFlowBox SelectionModeNone
  containerAdd noteList listFlowBox

  editNoteBox <- new Box [#orientation := OrientationVertical, #expand := False]
  widgetSetMarginLeft editNoteBox 5

  createButton <- new Button [#label := "Create"]
  widgetSetSizeRequest createButton 120 40
  createAlign <- alignmentNew 0.5 0 0 0
  #add createAlign createButton
  createButtonBox <- new Box [#expand := False]
  #add createButtonBox createButton
  #add editNoteBox createAlign

  #add mainBox editNoteBox

  titleEntryLabel <- new Label [#label := "Title: "]
  widgetSetMarginTop titleEntryLabel 10
  #add editNoteBox titleEntryLabel

  titleEntry <- entryNew
  entrySetMaxLength titleEntry 250
  widgetSetSizeRequest titleEntry 400 30
  widgetSetMarginTop titleEntry 5
  noteTitleBox <- new Box [#expand := False]
  widgetSetSizeRequest noteTitleBox 400 30
  #add noteTitleBox titleEntry
  #add editNoteBox noteTitleBox

  textViewLabel <- new Label [#label := "Text: "]
  widgetSetMarginTop textViewLabel 10
  #add editNoteBox textViewLabel

  textView <- textViewNew
  noteTextScrolled <- scrolledWindowNew noAdjustment noAdjustment
  scrolledWindowSetPolicy noteTextScrolled PolicyTypeAutomatic PolicyTypeAutomatic
  widgetSetSizeRequest noteTextScrolled 400 300
  widgetSetMarginTop textView 5
  noteTextBox <- new Box [#expand := False]
  #add noteTextScrolled textView
  #add noteTextBox noteTextScrolled
  #add editNoteBox noteTextBox

  saveButton <- new Button [#label := "Save"]
  widgetSetSizeRequest saveButton 200 40

  saveButtonBox <- new Box [#expand := False]
  #add saveButtonBox saveButton

  deleteButton <- new Button [#label := "Delete"]
  widgetSetSizeRequest deleteButton 200 40
  deleteButtonBox <- new Box [#expand := False]
  #add deleteButtonBox deleteButton

  saveDeleteBox <- new Box [#expand := False]
  widgetSetMarginTop saveDeleteBox 5
  #add saveDeleteBox saveButtonBox
  #add saveDeleteBox deleteButtonBox
  #add editNoteBox saveDeleteBox

  createFlowbox listFlowBox stateLabel titleEntry textView
  on updateButton #clicked (createFlowbox listFlowBox stateLabel titleEntry textView)
  on createButton #clicked (createNote titleEntry noteTextScrolled stateLabel listFlowBox)
  on saveButton #clicked (saveNote titleEntry noteTextScrolled stateLabel listFlowBox textView)
  on deleteButton #clicked (deleteNote listFlowBox stateLabel titleEntry noteTextScrolled)

  return win

main :: IO ()
main = do
  _ <- GI.init Nothing
  win <- mainWindow
  _ <- onWidgetDestroy win mainQuit
  widgetShowAll win
  GI.main
