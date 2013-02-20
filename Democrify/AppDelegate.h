#import <Cocoa/Cocoa.h>
#import <CocoaLibSpotify/CocoaLibSpotify.h>

NSString *getNextTrack();
void setResourcePath(NSString* path);

@interface AppDelegate : NSObject <NSApplicationDelegate, SPSessionDelegate> {
@private
    IBOutlet NSMenu *democriMenu;
    IBOutlet NSMenu *playlistsMenu;
    IBOutlet NSImageView *democriIcon;
    
    NSStatusItem *democriItem;
    NSImage *democriStatusIcon;
    
	NSTextField *__weak userNameField;
	NSSecureTextField *__weak passwordField;
   
    NSObject *playObserver;

    
    SPPlaybackManager *playbackManager;

}

@property (assign) IBOutlet NSWindow *window;
@property (weak) IBOutlet NSTextField *userNameField;
@property (weak) IBOutlet NSSecureTextField *passwordField;

- (IBAction)quitFromAction:(id)sender;
- (IBAction)login:(id)sender;

- (void)playTrack:(NSString *)trackId;
- (void)updateMenu;
- (void)updatePlaylists;
- (void)loadPlaylistIntoQueue:(id)sender;

- (IBAction)skipTrack:(id)sender;
- (IBAction)playButton:(id)sender;
- (IBAction)pauseButton:(id)sender;

#pragma mark -

@property (nonatomic, readwrite, strong) SPPlaybackManager *playbackManager;


@end
