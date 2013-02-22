//
//  AppDelegate.m
//  Empty CocoaLibSpotify Project
//
//  Created by Daniel Kennett on 02/08/2012.
/*
 Copyright (c) 2011, Spotify AB
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 * Neither the name of Spotify AB nor the names of its contributors may
 be used to endorse or promote products derived from this software
 without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL SPOTIFY AB BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDxING NEGLIGENCE
 OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#import "AppDelegate.h"
#import <CocoaLibSpotify/CocoaLibSpotify.h>

#define SP_LIBSPOTIFY_DEBUG_LOGGING 0

#include "appkey.c"

@implementation AppDelegate

@synthesize userNameField;
@synthesize passwordField;
@synthesize playbackManager;

-(void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	// Insert code here to initialize your application
	NSString *userAgent = [[[NSBundle mainBundle] infoDictionary] valueForKey:(__bridge NSString *)kCFBundleIdentifierKey];
	NSData *appKey = [NSData dataWithBytes:&g_appkey length:g_appkey_size];

    
	NSError *error = nil;
	[SPSession initializeSharedSessionWithApplicationKey:appKey
											   userAgent:userAgent
										   loadingPolicy:SPAsyncLoadingManual
												   error:&error];
	if (error != nil) {
		NSLog(@"CocoaLibSpotify init failed: %@", error);
		abort();
	}
    
	[[SPSession sharedSession] setDelegate:self];
	self.playbackManager = [[SPPlaybackManager alloc] initWithPlaybackSession:[SPSession sharedSession]];
    
    //KVO current track
    [self.playbackManager addObserver:self forKeyPath:@"currentTrack" options:NSKeyValueObservingOptionNew context:nil];


}

-(void)awakeFromNib {
    NSBundle *bundle = [NSBundle mainBundle];

    setResourcePath([bundle resourcePath]);
    
    // load login sheet image
    NSImage *democriIconImage = [[NSImage alloc] initWithContentsOfFile:[[NSBundle mainBundle] pathForResource:@"democrify" ofType:@"png"]];
    [democriIcon setImage:democriIconImage];

    // Create status bar icon
    democriItem = [[NSStatusBar systemStatusBar] statusItemWithLength:NSSquareStatusItemLength];
    
    democriStatusIcon = [[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"statusicon" ofType:@"tiff"]];
    
    [democriItem setImage:democriStatusIcon];
    [democriItem setMenu:democriMenu];
    [democriMenu setAutoenablesItems:NO];
    [playlistsMenu setAutoenablesItems:NO];
    [playlistsMenu removeAllItems];
    
    [[democriMenu itemWithTitle:@"Playlists"] setEnabled:NO]; // No playlists before login!
    
    [[democriMenu itemWithTitle:@"Play"] setEnabled:NO]; //No play before login!
    [[democriMenu itemWithTitle:@"Pause"] setHidden:YES]; //No pause before play!
    [[democriMenu itemWithTitle:@"Skip"] setEnabled:NO]; //No skip before play!
}

-(NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication *)sender {
	// When quitting, you should logout and wait for logout completion before terminating.
	if ([SPSession sharedSession].connectionState == SP_CONNECTION_STATE_LOGGED_OUT ||
		[SPSession sharedSession].connectionState == SP_CONNECTION_STATE_UNDEFINED)
		return NSTerminateNow;

	[[SPSession sharedSession] logout:^{
		[[NSApplication sharedApplication] replyToApplicationShouldTerminate:YES];
	}];
	return NSTerminateLater;
}

#pragma mark -
#pragma mark SPSessionDelegate Methods

-(void)sessionDidLoginSuccessfully:(SPSession *)aSession {
	// Called after a successful login.
    [[democriMenu itemWithTitle:@"Play"] setEnabled:YES];
    [self updateMenu];
    [self.window close];
    
    // initialize playlists
    [[democriMenu itemWithTitle:@"Playlists"] setEnabled:YES];

    [self updatePlaylists];
}

-(void)session:(SPSession *)aSession didFailToLoginWithError:(NSError *)error {
	// Called after a failed login.
    [NSApp presentError:error
         modalForWindow:self.window
               delegate:nil
     didPresentSelector:nil
            contextInfo:nil];
}

-(void)sessionDidLogOut:(SPSession *)aSession; {
	// Called after a logout has been completed.
}

-(void)session:(SPSession *)aSession didGenerateLoginCredentials:(NSString *)credential forUserName:(NSString *)userName {

	// Called when login credentials are created. If you want to save user logins, uncomment the code below.
	/*
	 NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
	 NSMutableDictionary *storedCredentials = [[defaults valueForKey:@"SpotifyUsers"] mutableCopy];

	 if (storedCredentials == nil)
	 storedCredentials = [NSMutableDictionary dictionary];

	 [storedCredentials setValue:credential forKey:userName];
	 [defaults setValue:storedCredentials forKey:@"SpotifyUsers"];
	 */
}

-(void)session:(SPSession *)aSession didEncounterNetworkError:(NSError *)error; {
	if (SP_LIBSPOTIFY_DEBUG_LOGGING != 0)
		NSLog(@"CocoaLS NETWORK ERROR: %@", error);
}

-(void)session:(SPSession *)aSession didLogMessage:(NSString *)aMessage; {
	if (SP_LIBSPOTIFY_DEBUG_LOGGING != 0)
		NSLog(@"CocoaLS DEBUG: %@", aMessage);
}

-(void)sessionDidChangeMetadata:(SPSession *)aSession; {
	// Called when metadata has been updated somewhere in the
	// CocoaLibSpotify object model. You don't normally need to do
	// anything here. KVO on the metadata you're interested in instead.
}

-(void)session:(SPSession *)aSession recievedMessageForUser:(NSString *)aMessage; {
	// Called when the Spotify service wants to relay a piece of information to the user.
	[[NSAlert alertWithMessageText:aMessage
					 defaultButton:@"OK"
				   alternateButton:@""
					   otherButton:@""
		 informativeTextWithFormat:@"This message was sent to you from the Spotify service."] runModal];
}

#pragma mark -
#pragma mark Playback

//automatically get the next track when the current one stops
- (void)observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary *)change context:(void *)context {
    if ([keyPath isEqualToString:@"currentTrack"]) {
        // don't even ask :)
        if (![[change objectForKey:NSKeyValueChangeNewKey] isKindOfClass:[SPTrack class]]) {
            [self playTrack:getNextTrack()];
        }
    }
}

- (void)playTrack:(NSString *)trackId {
	    
	if ([trackId length] > 0) {
		
		NSURL *trackURL = [NSURL URLWithString:[NSString stringWithFormat:@"spotify:track:%@", trackId]];
		[[SPSession sharedSession] trackForURL:trackURL callback:^(SPTrack *track) {
			if (track != nil) {
				
				[SPAsyncLoading waitUntilLoaded:track timeout:kSPAsyncLoadingDefaultTimeout then:^(NSArray *tracks, NSArray *notLoadedTracks) {
					[self.playbackManager playTrack:track callback:^(NSError *error) {
						if (error)
							[self.window presentError:error];
                        
					}];
				}];
			}
		}];
		return;
	}
	NSBeep();
}

- (void)updateMenu {
    if (self.playbackManager.playbackSession.playing) {
        [[democriMenu itemWithTitle:@"Play"] setHidden:YES];
        [[democriMenu itemWithTitle:@"Pause"] setHidden:NO];
        [[democriMenu itemWithTitle:@"Skip"] setEnabled:YES];
    } else {
        [[democriMenu itemWithTitle:@"Play"] setHidden:NO];
        [[democriMenu itemWithTitle:@"Pause"] setHidden:YES];
        [[democriMenu itemWithTitle:@"Skip"] setEnabled:NO];
    }
}

- (void)updatePlaylists {

    // load and display playlists in the submenu
    SPSession *session = [SPSession sharedSession];

    
    [SPAsyncLoading waitUntilLoaded:session timeout:10.0 then:^(NSArray *loadedItems, NSArray *notLoadedItems) {
        [SPAsyncLoading waitUntilLoaded:session.userPlaylists timeout:10.0 then:^(NSArray *loadedItems, NSArray *notLoadedItems) {
            SPPlaylistContainer *container = [SPSession sharedSession].userPlaylists;
            NSArray *playlists = container.flattenedPlaylists;
            // ^These likely won't have been loaded yet.
            [SPAsyncLoading waitUntilLoaded:playlists timeout:60.0 then:^(NSArray *loadedItems, NSArray *notLoadedItems) {
                // Some playlists will have loaded by now. If the user has a lot of playlists and/or they have a lot of tracks, it'll take a lot longer than 10 seconds.
                for (SPPlaylist *p in loadedItems){
                    [playlistsMenu addItemWithTitle:p.name action:@selector(loadPlaylistIntoQueue:) keyEquivalent:@""];
                    [[playlistsMenu itemWithTitle:p.name] setRepresentedObject:p];
                    [[playlistsMenu itemWithTitle:p.name] setEnabled:YES];
                }
            }];
        }];
    }];
}

- (void)loadPlaylistIntoQueue:(id)sender {
    SPPlaylist *p = [sender representedObject];

    //get NSStrings from this stuff
    NSMutableArray *tracksM = [NSMutableArray arrayWithCapacity:[p.items count]];
    for (SPPlaylistItem *i in p.items){
        if (i.itemURLType == SP_LINKTYPE_TRACK) {
            NSString *iUrl = [i.itemURL absoluteString];
            [tracksM addObject:iUrl];
        }
    }
    
    NSArray *tracks = [[NSArray alloc] initWithArray:tracksM];

    loadPlaylist(tracks);
}

// Interface actions

- (IBAction)playButton:(id)sender {
    if ((!self.playbackManager.playbackSession.playing) && (self.playbackManager.currentTrack)) {
        self.playbackManager.playbackSession.playing = true;
        [self updateMenu];
    } else {
        [self playTrack:getNextTrack()];
        self.playbackManager.playbackSession.playing = true;
        [self updateMenu];
    }
}

// Skip a track
- (IBAction)skipTrack:(id)sender {
    [self playTrack:getNextTrack()];
}

// Pause playback
- (IBAction)pauseButton:(id)sender {
    self.playbackManager.playbackSession.playing = false;
    [self updateMenu];
}

// Empty the queue
- (IBAction)emptyQueue:(id)sender {
    extEmptyQueue();
}

//Launch user UI
- (IBAction)userUI:(id)sender {
    [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"http://localhost:8686/"]];
}

//Launch admin UI
- (IBAction)adminUI:(id)sender {
    [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"http://localhost:8686/admin"]];
}


- (IBAction)login:(id)sender {
	
	// Invoked by clicking the "Login" button in the UI.
	
	if ([[userNameField stringValue] length] > 0 &&
		[[passwordField stringValue] length] > 0) {
		
		[[SPSession sharedSession] attemptLoginWithUserName:[userNameField stringValue]
												   password:[passwordField stringValue]];
	} else {
		NSBeep();
	}
}

// Invoked by clicking the "Quit" button in the UI.
- (IBAction)quitFromAction:(id)sender {
	[NSApp terminate:self];
}

@end
