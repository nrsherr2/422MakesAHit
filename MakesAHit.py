## Group 05

import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import time
import pandas as pd

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

# List of attributes we will store per track
keys = ['name', 'artist', 'album', 'explicit', 'popularity', 'acousticness', 'danceability', 'energy',
        'instrumentalness', 'key', 'liveness', 'loudness', 'mode',
        'speechiness', 'tempo', 'time_signature', 'valence', 'genre']


# Client Authorization
def connect_to_API():
    # client: c27affe22ea041da8d4f6c113f73a296
    # secret: 4e1a8c6c8d424595a9479e2c288f6582
    client_credentials_manager = SpotifyClientCredentials('c27affe22ea041da8d4f6c113f73a296', '4e1a8c6c8d424595a9479e2c288f6582')
    return spotipy.Spotify(client_credentials_manager=client_credentials_manager)


# Copy spotify playlist into a usable data frame
def get_genre(artist_genres):
    # populate our dictionary
    genre_count = {}
    for g in genre_list:
        genre_count[g] = 0
    # look through the genres
    for genre in artist_genres:
        for g in genre_list:
            if g in genre:
                genre_count[g] += 1
    best_count = 0
    true_genre = ""
    for g in genre_count:
        if genre_count[g] > best_count:
            true_genre = g
            best_count = genre_count[g]
    return true_genre


# Copy spotify playlist into a usable data frame
def populate(tdf, training_playlist_batch, sp):
    for item in training_playlist_batch['items']:
        # track fields
        name = item['track']['name']
        artist = item['track']['artists'][0]['name']
        artist_object = sp.artist(item['track']['artists'][0]['id'])
        artist_genres = artist_object['genres']
        genre = get_genre(artist_genres)
        album = sp.album(item['track']['album']['id'])['name']

        # track attributes
        explicit = item['track']['explicit']
        popularity = item['track']['popularity']
        audio_features = sp.audio_features(item['track']['id'])
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

        # create a list to represent a single song entry
        song = [name, artist, album, explicit, popularity, acousticness, danceability, energy, instrumentalness,
                key, liveness, loudness, mode,
                speechiness, tempo, time_signature, valence, genre]

        # hacky way to merge this entry into the overall data frame
        df = {}
        for i in range(0, len(keys)):
            key = keys[i]
            df[key] = song[i]

        # add to the tdf
        tdf = tdf.append(df, ignore_index=True)

    return tdf


# Start of execution
def main():
    sp = connect_to_API()

    # Dataframe reprentation of playlist for training
    tdf = pd.DataFrame(columns=keys)

    # Batch API requests, can only get 100 songs at a time from playlist
    total_tracks = 544
    current_index = 0

    while current_index < total_tracks:
        # Get playlist for training "CSC 422 Final Playlist"
        training_playlist_batch = sp.user_playlist_tracks('1280109077', '2XfT7nvDpRuwPPPXFrDLTK', offset=current_index)
        tdf = populate(tdf, training_playlist_batch, sp)
        current_index += 100
        print(current_index)
        time.sleep(5)

    # verify output
    print(tdf)

    # export to csv (use later)
    tdf.to_csv('spotify_training_data.csv')


# necessary to start execution at main
if __name__ == "__main__":
    main()
