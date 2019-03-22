## Group 05

import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import pandas as pd

# Client Authorization
# client: c27affe22ea041da8d4f6c113f73a296
# secret: 4e1a8c6c8d424595a9479e2c288f6582
client_credentials_manager = SpotifyClientCredentials('c27affe22ea041da8d4f6c113f73a296', '4e1a8c6c8d424595a9479e2c288f6582')
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

# Get playlist for training "CSC 422 Final Playlist"
# TODO: batching
training_playlist = sp.user_playlist_tracks('1280109077', '2XfT7nvDpRuwPPPXFrDLTK')

# List of genres for classification
genre_list = ["acoustic",
    "afrobeat",
    "alt-rock",
    "alternative",
    "ambient",
    "anime",
    "black-metal",
    "bluegrass",
    "blues",
    "bossanova",
    "brazil",
    "breakbeat",
    "british",
    "cantopop",
    "chicago-house",
    "children",
    "chill",
    "classical",
    "club",
    "comedy",
    "country",
    "dance",
    "dancehall",
    "death-metal",
    "deep-house",
    "detroit-techno",
    "disco",
    "disney",
    "drum-and-bass",
    "dub",
    "dubstep",
    "edm",
    "electro",
    "electronic",
    "emo",
    "folk",
    "forro",
    "french",
    "funk",
    "garage",
    "german",
    "gospel",
    "goth",
    "grindcore",
    "groove",
    "grunge",
    "guitar",
    "happy",
    "hard-rock",
    "hardcore",
    "hardstyle",
    "heavy-metal",
    "hip-hop",
    "holidays",
    "honky-tonk",
    "house",
    "idm",
    "indian",
    "indie",
    "indie-pop",
    "industrial",
    "iranian",
    "j-dance",
    "j-idol",
    "j-pop",
    "j-rock",
    "jazz",
    "k-pop",
    "kids",
    "latin",
    "latino",
    "malay",
    "mandopop",
    "metal",
    "metal-misc",
    "metalcore",
    "minimal-techno",
    "movies",
    "mpb",
    "new-age",
    "new-release",
    "opera",
    "pagode",
    "party",
    "philippines-opm",
    "piano",
    "pop",
    "pop-film",
    "post-dubstep",
    "power-pop",
    "progressive-house",
    "psych-rock",
    "punk",
    "punk-rock",
    "r-n-b",
    "rainy-day",
    "reggae",
    "reggaeton",
    "road-trip",
    "rock",
    "rock-n-roll",
    "rockabilly",
    "romance",
    "sad",
    "salsa",
    "samba",
    "sertanejo",
    "show-tunes",
    "singer-songwriter",
    "ska",
    "sleep",
    "songwriter",
    "soul",
    "soundtracks",
    "spanish",
    "study",
    "summer",
    "swedish",
    "synth-pop",
    "tango",
    "techno",
    "trance",
    "trip-hop",
    "turkish",
    "work-out",
"world-music"]

keys = ['name', 'artist', 'album_title', 'explicit', 'popularity', 'acousticness', 'danceability', 'energy',
        'instrumentalness', 'key', 'liveness', 'loudness', 'mode',
        'speechiness', 'tempo', 'time_signature', 'valence', 'genre']

# Dataframe reprentation of playlist
tdf = pd.DataFrame(columns = keys)

# Build Training Set
for item in training_playlist['items']:
    # track fields
    name = item['track']['name']
    artist = item['track']['artists'][0]['name']
    artist_id = item['track']['artists'][0]['id']
    artist_object = sp.artist(artist_id)
    artist_genres = artist_object['genres']
    # TODO: find most popular genre out of list of artist's gengres
    genre = '<<TBD>>'
    album_id = item['track']['album']['id']
    album_object = sp.album(album_id)
    album_title = album_object['name']

    # track attributes
    explicit = item['track']['explicit']
    popularity = item['track']['popularity']
    track_id = item['track']['id']
    audio_features = sp.audio_features(track_id)
    acousticness = audio_features[0]['acousticness']
    danceability = audio_features[0]['danceability']
    energy = audio_features[0]['energy']
    instrumentalness = audio_features[0]['instrumentalness']
    key = audio_features[0]['key']
    liveness = audio_features[0]['liveness']
    loudness = audio_features[0]['loudness']
    mode = audio_features[0]['mode']
    speechiness = audio_features[0]['speechiness']
    tempo = audio_features[0]['tempo']
    time_signature = audio_features[0]['time_signature']
    valence = audio_features[0]['valence']

    # create a list to represent a single song entry in the trainig_df(tdf)
    song = [name, artist, album_title, explicit, popularity, acousticness, danceability, energy, instrumentalness, key, liveness, loudness, mode,
          speechiness, tempo, time_signature, valence, genre]

    df = {}
    for i in range(0,len(keys)):
        key = keys[i]
        df[key] = song[i]

    # add to the tdf
    tdf = tdf.append(df, ignore_index=True)

print(tdf)

