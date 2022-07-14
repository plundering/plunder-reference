Chat
====

As a demo, we should build a simple chat application with multi-media
support.

    {-
        Mark: utf8-encoded Markdown.
        Text: utf8-encoded Monospace text
        Webm: .webm video
        Webp: .webp image
        Opus: .opus audio
        Quot: Quote another message

        - `Text` Pages must all be 80x60 w/ explicit line feed characters
          terminating pages.

        - `Quot` messages include the entire reply tree and may be from
          other channels.
    -}
    data MsgBody
        = Quot Pin-Msg
        | Mark Pin-Bar
        | Text Pin-Bar
        | Webm Pin-Bar
        | WebP Pin-Bar
        | Opus Pin-Bar

    data Msg = MSG
        { from      :: Who
        , chan      :: Who
        , when      :: When
        , body      :: MsgBody
        , reply     :: (Maybe Pin-Msg)
        , signature :: Bar
        }

    data History = [Msg]

Something like the above.

Don't both with DDoS prevention or chunked loading.  This is a demo and
a proof-of-concept.  For example, if a client sents a long string of
huges messages with huge reply chains, that's going to cause problems.

-   Every channel has it's own ed25519 key.  A single node manages
    each channel.

-   Every user has it's own ed25519 key.

Channels and users are not explicitly named, clients can set their own
names for channels and users.

Every machine runs a single user and hosts a set of channels.  The user
is subscribed to a set of channels, some run locally and some alien.

Channels have a simple API:

- Channels have whitelists.
- Channel whitelists updatable by owner.
- User removed from whitelist can no longer send messages.
- User removed from whitelist no longer gets updates.
- User removed from whitelist can't access library.
- User removed from whitelist messages are retained.

- Users join channels.
- Users download history from library.
- Library updated hourly.
- Library index is a row of hashes.
- Users subscribe to new updates.
- Library whitelist matches join whitelist.

- Messages since last library update are sent right away.
- Updates are best effort (cancelled after library update)
- Users submit signed messages.
- The channel validates messages.
- Channel conses message to list.
- New message distributed as update.
